package Catalyst::Controller::AutoAssets;
use strict;
use warnings;

our $VERSION = 0.10;

use Moose;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller' }

use Path::Class 0.32 qw( dir file );
use Fcntl qw( :DEFAULT :flock :seek F_GETFL );
use File::stat qw(stat);
use Catalyst::Utils;

require Digest::SHA1;
require MIME::Types;
require Module::Runtime;

# Directories to include
has 'include', is => 'ro', isa => 'Str|ArrayRef[Str]', required => 1;  

# The type of asset. 'js' and 'css' produce single-file content, while
# 'directory' host the directory structure of the original/include files
my @valid_types = qw(js css directory);
has 'type', is => 'ro', isa => 'Str', required => 1;

# Applies only to js or css, if true, tries to minify files
has 'minify', is => 'ro', isa => 'Bool', default => sub{0};

# Whether or not to make the current asset available via 307 redirect to the
# real, current checksum/fingerprint asset path
has 'current_redirect', is => 'ro', isa => 'Bool', default => sub{1};

# What string to use for the 'current' redirect
has 'current_alias', is => 'ro', isa => 'Str', default => sub { 'current' };

# Max number of seconds before recalculating the fingerprint (sha1 checksum)
# regardless of whether or not the mtime has changed. 0 means infinite/disabled
has 'max_fingerprint_calc_age', is => 'ro', isa => 'Int', default => sub {0};

# Max number of seconds to wait to obtain a lock (to be thread safe)
has 'max_lock_wait', is => 'ro', isa => 'Int', default => 120;

has 'cache_control_header', is => 'ro', isa => 'Str', 
  default => sub { 'public, max-age=31536000, s-max-age=31536000' }; # 31536000 = 1 year

######################################


sub BUILD {
  my $self = shift;
  
  my $type = $self->type;
  Catalyst::Exception->throw(
    "Invalid type '$type' = must be one of: " . join(', ', @valid_types)
  ) unless ($type ~~ @valid_types); # Would much rather be doing this with an 'Enum' type constraint
  
  Catalyst::Exception->throw("Must include at least one file/directory")
    unless ($self->include_count > 0);
  
  # The 'directory' type is a passthrough mode of operation
  if ($self->type eq 'directory') {
    Catalyst::Exception->throw(
      "'minify' isn't allowed with 'directory' asset type"
    ) if ($self->minify);
    
    Catalyst::Exception->throw(
      "Only one include directory is allowed with 'directory' asset type"
    ) unless ($self->include_count == 1);
    
    my ($root) = $self->includes;
    
    Catalyst::Exception->throw("Bad include path '$root'")
      unless (-d $root);
  }
  else {
    Catalyst::Exception->throw("No minifier available")
      if($self->minify && ! $self->minifier);
  }
  
  my $work_dir = dir($self->work_dir);
  $work_dir->mkpath($self->_app->debug) unless (-d $work_dir);
  
  $self->prepare_asset;
}

#############################
sub index :Path {
  my ( $self, $c, $arg ) = @_;
  
  $self->prepare_asset;
  
  return $c->detach('cur_request') if (
    $self->current_redirect &&
    ($arg eq $self->current_alias || $arg eq $self->current_alias . '.' . $self->type)
  );
  
  return $self->is_dir ? $c->detach('dir_request') : $c->detach('file_request');
}

sub cur_request :Private {
  my ( $self, $c, $arg, @args ) = @_;
  
  $c->response->header( 'Cache-Control' => 'no-cache' );
  $c->response->redirect(join('/',$self->asset_path,@args), 307);
  return $c->detach;
}

sub file_request :Private {
  my ( $self, $c, @args ) = @_;
  
  my $want_asset = join('/',@args);

  my $asset = $self->asset_name;
  return $self->unknown_asset($c,$want_asset) unless ($asset eq $want_asset);
  
  # Let browsers cache forever because we're a CAS path! content will always be current
  $c->response->header(
    'Content-Type' => $self->asset_content_type,
    'Cache-Control' => $self->cache_control_header
  ); 
  
  return $c->response->body( $self->asset_fh );
}

sub dir_request :Private {
  my ( $self, $c, $sha1, @args ) = @_;
  
  my $want_asset = join('/',$sha1,@args);
  
  return $self->unknown_asset($c,$want_asset) unless ($sha1 eq $self->asset_name);
  
  my $File = $self->_get_sub_file(@args);
  return $self->unknown_asset($c,$want_asset) unless (-f $File);
  
  $c->response->header(
    'Content-Type' => $self->_ext_to_type($File),
    'Cache-Control' => $self->cache_control_header
  );
  
  return $c->response->body( $File->openr );
}
############################

sub is_dir { return (shift)->type eq 'directory' ? 1 : 0 }

has 'minifier', is => 'ro', isa => 'Maybe[CodeRef]', lazy => 1, default => sub {
  my $self = shift;
  if($self->type eq 'css') {
    Module::Runtime::require_module('CSS::Minifier');
    return sub { CSS::Minifier::minify(@_) };
  }
  elsif($self->type eq 'js') {
    Module::Runtime::require_module('JavaScript::Minifier');
    return sub { JavaScript::Minifier::minify(@_) };
  }
  else {
    return undef;
  }
};

has 'asset_content_type', is => 'ro', isa => 'Str', lazy => 1, default => sub {
  my $self = shift;
  if ($self->type eq 'js') {
    return 'text/javascript';
  }
  elsif ($self->type eq 'css') {
    return 'text/css';
  }
  else {
    return undef;
  }
};

has 'work_dir', is => 'ro', lazy => 1, default => sub {
  my $self = shift;
  my $c = $self->_app;
  
  my $tmpdir = Catalyst::Utils::class2tempdir($c)
    || Catalyst::Exception->throw("Can't determine tempdir for $c");
    
  my $dir = dir($tmpdir, "AutoAssets",  $self->action_namespace($c));
  return $dir->resolve;
};

has 'built_file', is => 'ro', lazy => 1, default => sub {
  my $self = shift;
  my $filename = 'built.' . $self->type;
  return file($self->work_dir,$filename);
};

has 'fingerprint_file', is => 'ro', lazy => 1, default => sub {
  my $self = shift;
  return file($self->work_dir,'fingerprint');
};

has 'lock_file', is => 'ro', lazy => 1, default => sub {
  my $self = shift;
  return file($self->work_dir,'lockfile');
};

has 'MimeTypes', is => 'ro', lazy => 1, default => sub {
  my $self = shift;
  return MIME::Types->new( only_complete => 1 );
};

# looks up the correct MIME type for the current file extension
# (adapted from Static::Simple)
sub _ext_to_type {
  my ( $self, $full_path ) = @_;
  my $c = $self->_app;

  if ( $full_path =~ /.*\.(\S{1,})$/xms ) {
    my $ext = $1;
    my $type = $self->MimeTypes->mimeTypeOf( $ext );
    if ( $type ) {
      return ( ref $type ) ? $type->type : $type;
    }
    else {
      return 'text/plain';
    }
  }
  else {
    return 'text/plain';
  }
}

# for directory type only:
sub _get_sub_file {
  my $self = shift;
  my ($root) = $self->includes;
  return file($root,@_);
}

sub includes {
  my $self = shift;
  return ref $self->include ? @{$self->include} : $self->include;
}

sub include_count {
  my $self = shift;
  return ref $self->include ? scalar @{$self->include} : 1;
}

sub get_include_files {
  my $self = shift;

  my @files = ();
  for my $inc ($self->includes) {
    $inc = dir($inc)->absolute;
    if(-f $inc) {
      push @files, $inc;
    }
    elsif(-d $inc) {
      dir($inc)->recurse(
        preorder => 1,
        depthfirst => 1,
        callback => sub {
          my $child = shift;
          push @files, $child->absolute unless ($child->is_dir);
        }
      );
    }
    else {
      die "AutoAsset include path '$inc' not found";
    }
  }
  
  # force consistent ordering of files:
  @files = sort @files;
   
  return \@files;
}

has 'last_fingerprint_calculated', is => 'rw', isa => 'Maybe[Int]', default => sub{undef};

has 'built_mtime', is => 'rw', isa => 'Maybe[Str]', default => sub{undef};
sub get_built_mtime {
  my $self = shift;
  return -f $self->built_file ? stat($self->built_file)->mtime : undef;
}

has 'inc_mtimes', is => 'rw', isa => 'Maybe[Str]', default => undef;
sub get_inc_mtime_concat {
  my $self = shift;
  my $list = shift;
  return join('-', map { stat($_)->mtime } @$list );
}

sub calculate_fingerprint {
  my $self = shift;
  my $list = shift;
  # include both the include (source) and built (output) in the fingerprint:
  my $sha1 = $self->file_checksum(@$list,$self->built_file);
  $self->last_fingerprint_calculated(time) if ($sha1);
  return $sha1;
}

sub current_fingerprint {
  my $self = shift;
  return undef unless (-f $self->fingerprint_file);
  my $fingerprint = file($self->fingerprint_file)->slurp;
  return $fingerprint;
}

sub save_fingerprint {
  my $self = shift;
  my $fingerprint = shift or die "Expected fingerprint/checksum argument";
  return file($self->fingerprint_file)->spew($fingerprint);
}

sub calculate_save_fingerprint {
  my $self = shift;
  my $fingerprint = $self->calculate_fingerprint or return 0;
  return $self->save_fingerprint($fingerprint);
}

sub fingerprint_calc_current {
  my $self = shift;
  my $last = $self->last_fingerprint_calculated or return 0;
  return 1 if ($self->max_fingerprint_calc_age == 0); # <-- 0 means infinite
  return 1 if (time - $last < $self->max_fingerprint_calc_age);
  return 0;
}

sub prepare_asset {
  my $self = shift;
  
  file($self->built_file)->touch unless (-e $self->built_file);
  
  # For 'directory' only consider the mtime of the top directory and don't
  # read in all the files yet
  my $files = $self->is_dir ? [ $self->includes ] : $self->get_include_files;
  my $inc_mtimes = $self->get_inc_mtime_concat($files);
  my $built_mtime = $self->get_built_mtime;
  
  # Check cached mtimes to see if anything has changed. This is a lighter
  # first pass check than the fingerprint check which calculates a sha1 for
  # all the source files and existing built files
  return if (
    $self->fingerprint_calc_current &&
    $self->inc_mtimes eq $inc_mtimes && 
    $self->built_mtime eq $built_mtime
  );
  
  # Get the real list of files that we put off above for 'directory' assets
  $files = $self->get_include_files if ($self->is_dir);
  
  # --- Blocks for up to 2 minutes waiting to get an exclusive lock or dies
  $self->get_build_lock;
  # ---
  
  # Check the fingerprint:
  my $fingerprint = $self->calculate_fingerprint($files);
  my $cur_fingerprint = $self->current_fingerprint;
  if($fingerprint && $cur_fingerprint && $cur_fingerprint eq $fingerprint) {
    # If the mtimes changed but the fingerprint matches we don't need to regenerate. 
    # This will happen if another process just built the files while we were waiting 
    # for the lock and on the very first time after the application starts up
    $self->inc_mtimes($inc_mtimes);
    $self->built_mtime($built_mtime);
    return $self->release_build_lock;
  }
  
  # Need to do a rebuild:
  
  my $fd = file($self->built_file)->openw or die $!;
  if($self->is_dir) {
    # The built file is just a placeholder in the case of 'directory' type 
    # asset whose data is served from the original files
    my ($root) = $self->includes;
    my @relative = map { file($_)->relative($root) } @$files;
    $fd->write(join("\r\n",@relative) . "\r\n");
  }
  else {
    if($self->minify && $self->minifier) {
      foreach my $file (@$files) {
        open(INFILE, $file) or die $!;
        $self->minifier->( input => *INFILE, outfile => $fd );
        close INFILE;
        $fd->write("\r\n");
      }
    }
    else {
      $fd->write($_) for ( map { file($_)->slurp . "\r\n" } @$files );
    }
  }
  $fd->close;
  
  # Update the fingerprint (global) and cached mtimes (specific to the current process)
  $self->inc_mtimes($inc_mtimes);
  $self->built_mtime($self->get_built_mtime);
  $self->calculate_save_fingerprint;
  
  $self->_app->log->info("Built asset: " . $self->asset_path);
  
  # Release the lock and return:
  return $self->release_build_lock;
}



sub file_checksum {
  my $self = shift;
  my @files = @_;
  
  my $Sha1 = Digest::SHA1->new;
  foreach my $file (@files) {
    my $fh = file($file)->openr or die "$! : $file\n";
    $Sha1->addfile($fh);
    $fh->close;
  }
  
  return $Sha1->hexdigest;
}

sub asset_name {
  my $self = shift;
  my $sha1 = $self->current_fingerprint;
  return $self->is_dir ? $sha1 : $sha1 . '.' . $self->type;
  return  . '.' . $self->type;
}

sub asset_path {
  my ($self, @subpath) = @_;
  my $base = '/' . $self->action_namespace($self->_app) . '/' . $self->asset_name;
  return $base unless (scalar @subpath > 0);
  Catalyst::Exception->throw("Cannot use subpath with non directory asset")
    unless $self->is_dir;
  my $File = $self->_get_sub_file(@subpath);
  my $path = join('/',@subpath);
  Catalyst::Exception->throw("sub file '$path' not found") unless (-f $File);
  return join('/',$base,$path);
}




sub asset_fh {
  my $self = shift;

  my $file = $self->built_file;
  return undef unless (-f $file);
  
  my $fh = file($file)->openr or die "$! : $file\n";
  return $fh;
}

sub unknown_asset {
  my ($self,$c, $asset) = @_;
  $c->res->status(404);
  return $c->res->body("No such asset '$asset'");
}

sub get_build_lock_wait {
  my $self = shift;
  my $start = time;
  until($self->get_build_lock) {
    my $elapsed = time - $start;
    Catalyst::Exception->throw("AutoAssets: aborting waiting for lock after $elapsed")
      if ($elapsed >= $self->max_lock_wait);
    sleep 1;
  }
}

sub get_build_lock {
  my $self = shift;
  my $fname = $self->lock_file;
  sysopen(LOCKHANDLE, $fname, O_RDWR|O_CREAT|O_EXCL, 0644)
    or sysopen(LOCKHANDLE, $fname, O_RDWR)
    or die "Unable to create or open $fname\n";
  fcntl(LOCKHANDLE, F_SETFD, FD_CLOEXEC) or die "Failed to set close-on-exec for $fname";
  my $lockStruct= pack('sslll', F_WRLCK, SEEK_SET, 0, 0, $$);
  if (fcntl(LOCKHANDLE, F_SETLK, $lockStruct)) {
    my $data= "$$";
    syswrite(LOCKHANDLE, $data, length($data)) or die "Failed to write pid to $fname";
    truncate(LOCKHANDLE, length($data)) or die "Failed to resize $fname";
    # we do not close the file, so that we maintain the lock.
    return 1;
  }
  $self->release_build_lock;
  return 0;
}

sub release_build_lock {
  my $self = shift;
  close LOCKHANDLE;
}

sub end :Private {
  my ($self,$c) = @_;
  # Make sure we never keep a build lock past the end of a request:
  $self->release_build_lock;
}


1;

__END__

=pod

=head1 NAME

Catalyst::Controller::AutoAssets - Automatic asset serving via sha1-based URLs

=head1 VERSION

version 0.10

=head1 SYNOPSIS

In your controller:

  package MyApp::Controller::Assets::MyCSS;
  use parent 'Catalyst::Controller::AutoAssets';
  
  1;

Then, in your .conf:

  <Controller::Assets::MyCSS>
    include   root/my_stylesheets/
    type      css
    minify    1
  </Controller::Assets::MyCSS>

And in your .tt files:

  <head>
    <link rel="stylesheet" type="text/css" href="[% c.controller('Assets::MyCSS').asset_path %]" />
  </head>

Or, in static HTML:

  <head>
    <link rel="stylesheet" type="text/css" href="/assets/mycss/current.css" />
  </head>
  

=head1 DESCRIPTION

Fast, convenient serving of assets (css, javascript, etc) at URL path(s) containing a sha1 
checksum of the content. This is an alternative/supplement to L<Catalyst::Plugin::Static::Simple> or
external/webserver for serving of an application's "nearly static" content.

The benefit of serving files through CAS paths ("content-addressable storage" - same design used by Git) 
is that it automatically alleviates client caching issues while still allowing for 
maximum aggressive cache settings. Because URL paths contain the sha1 checksum of the data, 
browsers can safely cache the content forever because "changes" automatically become new URLs. 
If the content (CSS, JavaScript or other) is modified later on, the client browsers instantly 
see the new version.

This is particularly useful when deploying new versions of an application where client browsers
out in the network might have cached CSS and JavaScript from previous versions. Instead of asking 
users to hit "F5", everyone gets the new content automagically, with no intervention required. 
All you have to do is change the content; the module handles the rest.

This module also provides some optional extra features that are useful in both development and
production environments for automatically managing, minifying and deploying CSS and JavaScript assets.

=head1 CONFIG PARAMS

=head2 type

B<Required> - The asset type: C<directory>, C<css> or C<js>.

The C<directory> asset type works in a similar manner as Static::Simple to make some directory
structure accessible at a public URL. The root of the structure is made available at the URL path:

  <CONTROLLER_PATH>/<SHA1>/

L<MIME::Types> is used to set the C<Content-Type> HTTP header based on
the file extension (same as Static::Simple does).

Because the sha1 checksum changes automatically and is unknown in advance, the above Asset Path is made available
via the C<asset_path()> controller method for use in TT files and throughout the application.

The C<css> and C<js> types serve one automatically generated text file that is concatenated and
optionally minified from the include files. The single, generated file is made available at the URL 
Path:

  <CONTROLLER_PATH>/<SHA1>.js    # for 'js' type
  <CONTROLLER_PATH>/<SHA1>.css   # for 'css' type

The js/css types provide a bonus mode of operation to provide a simple and convenient way to 
manage groups of CSS and JavaScript files to be automatically deployed in the application. This
is also particularly useful during development. Production applications with their own management
and build process for CSS and JavaScript would simply use the C<directory> type.

=head2 include

B<Required> - String or ArrayRef. The path(s) on the local filesystem containing the source asset files. 
For C<directory> type this must be exactly one directory, while for C<css> and C<js> it can
be a list of directories. The C<include> directory becomes the root of the files hosted as-is
for the C<directory> type, while for C<css> and C<js> asset types it is the include files 
concatinated together (and possibly minified) to be served as the single file.

=head2 current_redirect

Whether or not to make the current asset available via 307 redirect to the
real, current checksum/fingerprint asset path. This is a pure HTTP mechanism of resolving the
asset path.

  <CONTROLLER_PATH>/current/      # for 'directory' type
  <CONTROLLER_PATH>/current.js    # for 'js' type
  <CONTROLLER_PATH>/current.css   # for 'css' type

For instance, you might reference a CSS file from a C<directory> asset C<Controller::Assets::ExtJS> 
using this URL path (i.e. href in an HTML C<link> tag):

  /assets/extjs/current/resources/css/ext-all.css

This path would redirect (HTTP 307) to the current asset/file path which would be something like:

  /assets/extjs/1512834162611db1fab246dfa87e3a37f68ed95f/resources/css/ext-all.css

The downside of this is that the server has to serve the non-cachable redirect every time, which 
partially defeats the performance benefits of this module (although the redirect is comparatively lightweight).

The other mechanism to find the current asset path is via the C<asset_path()> method, which returns
the current path outright and is the recommended usage, but is only available in locations where 
application controller methods can be called (like in TT files).

Defaults to true (1).

=head2 current_alias

Alias to use for the C<current_redirect>. Defaults to 'current' (which also implies 'current.js'/'current.css'
for C<js> and C<css> asset types).

=head2 minify

Whether or not to attempt to minify content for C<css> or C<js> asset types. This is a purely optional
convenience feature.

Defaults to false (0). Does not apply to the C<directory> asset type.

=head2 minifier

CodeRef used to minify the content when C<minify> is true. The default code is a pass-through to 
C<CSS::Minifier::minify()> for C<css> assets and C<JavaScript::Minifier::minify()> for C<js>. If
you want to override you must follow the same API as in those modules, using the C<input> and 
C<outfile> filehandle interface. See L<JavaScript::Minifier> and L<CSS::Minifier> for more details.

Does not apply to the C<directory> asset type.

=head2 work_dir

The directory where asset-specific files are generated and stored. This contains the checksum/fingerprint 
file, the lock file, and the built file. In the case of C<directory> assets the built file contains a manifest
of files and in the case of C<css> and C<js> assets it contains the actual asset content (concatenated and 
possibly minified)

Defaults to:

  <APP_TMPDIR>/AutoAssets/<CONTROLLER_PATH>/

=head2 max_lock_wait

Number of seconds to wait to obtain an exclusive lock when recalculating/regenerating. For thread-safety, when the system
needs to regenerate the asset (fingerprint and built file) it obtains an exclusive lock on the lockfile in the 
work_dir. If another thread/process already has a lock, the system will wait for up to C<max_lock_wait> seconds
before giving up and throwing an exception.

Note that this is only relevant when the source/include content changes while the app is running (which should never 
happen in a production environment).

Defaults to 120 seconds.

=head2 max_fingerprint_calc_age

Max number of seconds before recalculating the fingerprint of the content (sha1 checksum)
regardless of whether or not the mtime has changed. 0 means infinite/disabled.

For performance, once the system has calculated the checksum of the asset content it caches the mtime
of the include file(s) and verifies on each request to see if they have changed. If they have, it 
regenerates the asset on the fly (recalculates the checksum and concatenates and minifies (if enabled)
for C<css> and C<js> asset types). If C<max_fingerprint_calc_age> is set to a non-zero value, it will force the
system to regenerate at least every N seconds regardless of the mtime. This would only be needed in cases
where you are worried the content could change without changing the mtime which shouldn't be needed in
most cases.

Defaults to 0.

=head2 asset_content_type

The content type returned in the 'Content-Type' header. Defaults to C<text/css> or C<text/javascript>
for the C<css> and C<js> types respectively. 

Does not apply to C<directory> asset type. For files within C<directory> type assets, the Content-Type 
is set according to the file extension using L<MIME::Types>.

=head2 cache_control_header

The HTTP C<'Cache-Control'> header to return when serving assets. Defaults to the maximum 
aggressive value that should be honored by most browsers (1 year):

  public, max-age=31536000, s-max-age=31536000

=back

=head1 METHODS

=head2 is_dir

Returns true (1) if the asset type is C<directory> and false (0) if the type is C<css> or C<js>.

=head2 asset_path

Returns the current, public URL path to the asset:

  <CONTROLLER_PATH>/<SHA1>       # for 'directory' type
  <CONTROLLER_PATH>/<SHA1>.js    # for 'js' type
  <CONTROLLER_PATH>/<SHA1>.css   # for 'css' type

For C<directory> asset types, accepts an optional subpath argument to a specific file. For example,
if there was a file C<images/logo.gif> within the include directory, $c->controller('Foo::MyAsset')->asset_path('images/logo.gif')
might return:

  /foo/myasset/1512834162611d99fab246dfa87345a37f68ed95f/images/logo.gif

=head1 SEE ALSO

=over

=item L<Catalyst::Plugin::Assets>

=item L<Catalyst::Controller::VersionedURI>

=item L<Plack::Middleware::Assets>

=item L<Plack::Middleware::JSConcat>

=back

=head1 AUTHOR

Henry Van Styn <vanstyn@cpan.org>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2013 by IntelliTree Solutions llc.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
