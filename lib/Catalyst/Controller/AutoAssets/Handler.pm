package Catalyst::Controller::AutoAssets::Handler;
use strict;
use warnings;

our $VERSION = 0.11;

use Moose::Role;
use namespace::autoclean;

use Path::Class 0.32 qw( dir file );
use Fcntl qw( :DEFAULT :flock :seek F_GETFL );
use File::stat qw(stat);
use Catalyst::Utils;
use Time::HiRes qw(gettimeofday tv_interval);
use Storable qw(store retrieve);
use Try::Tiny;

require Digest::SHA1;
require MIME::Types;
require Module::Runtime;

has '_Controller' => (
  is => 'ro', required => 1,
  isa => 'Catalyst::Controller::AutoAssets',
  handles => [qw(
    _app action_namespace 
    unknown_asset 
  )],
);

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

# Whether or not to use stored state data across restarts to avoid rebuilding.
has 'persist_state', is => 'ro', isa => 'Bool', default => sub{0};

# Optional shorter checksum
has 'sha1_string_length', is => 'ro', isa => 'Int', default => sub{40};

# directory to use for relative includes (defaults to the Catalyst home dir);
# TODO: coerce from Str
has '_include_relative_dir', isa => 'Path::Class::Dir', is => 'ro', lazy => 1,
  default => sub { dir( (shift)->_app->config->{home} )->resolve };


######################################


sub BUILD {
  my $self = shift;
  
  # optionally initialize state data from the copy stored on disk for fast
  # startup (avoids having to always rebuild after every app restart):
  $self->_restore_state if($self->persist_state);

  my $type = $self->type;
  my %valid = map {$_=>1} @valid_types; # perl 5.8 doesn't like ~~ operator
  Catalyst::Exception->throw(
    "Invalid type '$type' = must be one of: " . join(', ', @valid_types)
  ) unless ($valid{$type}); # TODO: setup a real type constraint
  
  # init includes
  $self->includes;
  
  Catalyst::Exception->throw("Must include at least one file/directory")
    unless (scalar @{$self->includes} > 0);

  # if the user picks something lower than 5 it is probably a mistake (really, anything
  # lower than 8 is probably not a good idea. But the full 40 is probably way overkill)
  Catalyst::Exception->throw("sha1_string_length must be between 5 and 40")
    unless ($self->sha1_string_length >= 5 && $self->sha1_string_length <= 40);

  # The 'directory' type is a passthrough mode of operation
  if ($self->type eq 'directory') {
    Catalyst::Exception->throw(
      "'minify' isn't allowed with 'directory' asset type"
    ) if ($self->minify);
    
    # init dir_root:
    $self->dir_root;
  }
  else {
    Catalyst::Exception->throw("No minifier available")
      if($self->minify && ! $self->minifier);
  }
  
  # init work_dir:
  $self->work_dir;
  
  $self->prepare_asset;
}



sub request {
  my $self = shift;
  my ( $c, $arg ) = @_;
  
  return $self->cur_request(@_) if (
    $self->current_redirect &&
    ($arg eq $self->current_alias || $arg eq $self->current_alias . '.' . $self->type)
  );
  
  return $self->is_dir ? $self->dir_request(@_) : $self->file_request(@_);
}

sub file_request {
  my ( $self, $c, @args ) = @_;
  my $want_asset = join('/',@args);

  $self->prepare_asset;

  return $self->unknown_asset($c,$want_asset) unless ($self->asset_name eq $want_asset);
  
  # Let browsers cache forever because we're a CAS path! content will always be current
  $c->response->header(
    'Content-Type' => $self->asset_content_type,
    'Cache-Control' => $self->cache_control_header
  ); 
  
  return $c->response->body( $self->asset_fh );
}

sub cur_request  {
  my ( $self, $c, $arg, @args ) = @_;

  my $path = $self->_valid_subpath($c,@args);
  $self->prepare_asset($path);

  $c->response->header( 'Cache-Control' => 'no-cache' );
  $c->response->redirect(join('/',$self->asset_path,@args), 307);
  return $c->detach;
}

sub dir_request {
  my ( $self, $c, $sha1, @args ) = @_;

  my $path = $self->_valid_subpath($c,@args);
  $self->prepare_asset($path);

  return $self->unknown_asset($c) unless (
    $path && $sha1 eq $self->asset_name
  );

  my $meta = $self->subfile_meta->{$path}
    or die "Unexpected error - meta data missing for subfile '$path'!";

  $c->response->header(
    'Content-Type' => $meta->{content_type},
    'Cache-Control' => $self->cache_control_header
  );

  return $c->response->body( $meta->{file}->openr );
}
############################


has 'work_dir', is => 'ro', isa => 'Path::Class::Dir', lazy => 1, default => sub {
  my $self = shift;
  my $c = $self->_app;
  
  my $tmpdir = Catalyst::Utils::class2tempdir($c)
    || Catalyst::Exception->throw("Can't determine tempdir for $c");
    
  my $dir = dir($tmpdir, "AutoAssets",  $self->action_namespace($c));
  $dir->mkpath($self->_app->debug);
  return $dir->resolve;
};

has 'built_file', is => 'ro', isa => 'Path::Class::File', lazy => 1, default => sub {
  my $self = shift;
  my $filename = 'built.' . $self->type;
  return file($self->work_dir,$filename);
};

has 'fingerprint_file', is => 'ro', isa => 'Path::Class::File', lazy => 1, default => sub {
  my $self = shift;
  return file($self->work_dir,'fingerprint');
};

has 'lock_file', is => 'ro', isa => 'Path::Class::File', lazy => 1, default => sub {
  my $self = shift;
  return file($self->work_dir,'lockfile');
};



sub _valid_subpath {
  my ($self, $c, @path) = @_;
  return undef unless (scalar @path > 0);
  my $File = $self->dir_root->file(@path);
  return $self->unknown_asset($c) unless (-f $File);
  return join('/',@path); # <-- return path string because it is the lookup key
}

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



has 'work_dir', is => 'ro', isa => 'Path::Class::Dir', lazy => 1, default => sub {
  my $self = shift;
  my $c = $self->_app;
  
  my $tmpdir = Catalyst::Utils::class2tempdir($c)
    || Catalyst::Exception->throw("Can't determine tempdir for $c");
    
  my $dir = dir($tmpdir, "AutoAssets",  $self->action_namespace($c));
  $dir->mkpath($self->_app->debug);
  return $dir->resolve;
};






has 'MimeTypes', is => 'ro', isa => 'MIME::Types', lazy => 1, default => sub {
  my $self = shift;
  return MIME::Types->new( only_complete => 1 );
};


sub _resolve_subfile_content_type {
  my $self = shift;
  my $File = shift;
  my $content_type = $self->subfile_meta->{$File}->{content_type}
    or die "content_type not found in subfile_meta for $File!";
  return $content_type;
}

# CodeRef used to determine the Content-Type of each 'directory' subfile
has 'content_type_resolver', is => 'ro', isa => 'CodeRef', default => sub{ \&_ext_to_type };

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

has 'includes', is => 'ro', isa => 'ArrayRef', lazy => 1, default => sub {
  my $self = shift;
  my $rel = $self->_include_relative_dir;
  my @list = ref $self->include ? @{$self->include} : $self->include;
  return [ map {
    my $inc = file($_);
    $inc = $rel->file($inc) unless ($inc->is_absolute);
    $inc = dir($inc) if (-d $inc); #<-- convert to Path::Class::Dir
    $inc->resolve
  } @list ];
};

sub get_include_files {
  my $self = shift;

  my @files = ();
  for my $inc (@{$self->includes}) {
    if($inc->is_dir) {
      $inc->recurse(
        preorder => 1,
        depthfirst => 1,
        callback => sub {
          my $child = shift;
          push @files, $child->absolute unless ($child->is_dir);
        }
      );
    }
    else {
      push @files, $inc;
    }
  }
  
  # force consistent ordering of files:
  return [sort @files];
}



has 'last_fingerprint_calculated', is => 'rw', isa => 'Maybe[Int]', default => sub{undef};

has 'built_mtime', is => 'rw', isa => 'Maybe[Str]', default => sub{undef};
sub get_built_mtime {
  my $self = shift;
  return -f $self->built_file ? $self->built_file->stat->mtime : undef;
}

# inc_mtimes are the mtime(s) of the include files. For directory assets
# this is *only* the mtime of the top directory (see subfile_meta below)
has 'inc_mtimes', is => 'rw', isa => 'Maybe[Str]', default => undef;
sub get_inc_mtime_concat {
  my $self = shift;
  my $list = shift;
  return join('-', map { $_->stat->mtime } @$list );
}

# subfile_meta applies only to 'directory' assets. It is a cache of mtimes of
# individual files within the directory since 'inc_mtimes' only conatins the top
# directory. This is used to check for mtime changes on individual subfiles when
# they are requested. This is for performance since it would be too expensive to
# attempt to check all the mtimes on every request
has 'subfile_meta', is => 'rw', isa => 'HashRef', default => sub {{}};
sub set_subfile_meta {
  my $self = shift;
  my $list = shift;
  $self->subfile_meta({
    map { $_->relative($self->dir_root)->stringify => {
      file => $_,
      mtime => $_->stat->mtime,
      content_type => $self->content_type_resolver->($self,$_)
    } } @$list
  });
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
  my $fingerprint = $self->fingerprint_file->slurp;
  return $fingerprint;
}

sub save_fingerprint {
  my $self = shift;
  my $fingerprint = shift or die "Expected fingerprint/checksum argument";
  return $self->fingerprint_file->spew($fingerprint);
}

sub calculate_save_fingerprint {
  my $self = shift;
  my $fingerprint = $self->calculate_fingerprint(@_) or return 0;
  return $self->save_fingerprint($fingerprint);
}

sub fingerprint_calc_current {
  my $self = shift;
  my $last = $self->last_fingerprint_calculated or return 0;
  return 1 if ($self->max_fingerprint_calc_age == 0); # <-- 0 means infinite
  return 1 if (time - $last < $self->max_fingerprint_calc_age);
  return 0;
}

# -----
# Quick and dirty state persistence for faster startup
has 'persist_state_file', is => 'ro', isa => 'Path::Class::File', lazy => 1, default => sub {
  my $self = shift;
  return file($self->work_dir,'state.dat');
};

has '_persist_attrs', is => 'ro', isa => 'ArrayRef', default => sub{[qw(
 built_mtime
 inc_mtimes
 last_fingerprint_calculated
 subfile_meta
)]};

sub _persist_state {
  my $self = shift;
  return undef unless ($self->persist_state);
  my $data = { map { $_ => $self->$_ } @{$self->_persist_attrs} };
  store $data, $self->persist_state_file;
  return $data;
}

sub _restore_state {
  my $self = shift;
  return 0 unless (-f $self->persist_state_file);
  my $data;
  try {
    $data = retrieve $self->persist_state_file;
    $self->$_($data->{$_}) for (@{$self->_persist_attrs});
  }
  catch {
    $self->clear_asset; #<-- make sure no partial state data is used
    $self->_app->log->warn(
      'Failed to restore state from ' . $self->persist_state_file
    );
  };
  return $data;
}
# -----

# only applies to 'directory' asset type
has 'dir_root', is => 'ro', isa => 'Path::Class::Dir', lazy => 1, default => sub {
  my $self = shift;

  die "dir_root only applies to 'directory' asset types"
    unless ($self->is_dir);

  die "'directory' assets must have exactly one include path"
    unless (scalar @{$self->includes} == 1);

  my $dir = $self->includes->[0]->absolute;

  die "include path '$dir' is not a directory"
    unless ($dir->is_dir);

  return $dir;
};

sub _subfile_mtime_verify {
  my ($self, $path) = @_;
  my $File = $self->dir_root->file($path);

  # Check the mtime of the requested file to see if it has changed
  # and force a rebuild if it has. This is done because it is too
  # expensive to check all the subfile mtimes on every request, and
  # changes within files would not otherwise be caught since file
  # content changes do not update the parent directory mtime
  $self->clear_asset unless (
    exists $self->subfile_meta->{$path} &&
    $File->stat->mtime eq $self->subfile_meta->{$path}->{mtime}
  );
}

# force rebuild on next request/prepare_asset
sub clear_asset {
  my $self = shift;
  $self->inc_mtimes(undef);
}

sub _is_rebuild_required {
  my ($self, $inc_mtimes, $built_mtime) = @_;
  die "_is_rebuild_required(): missing arguments" unless ($built_mtime);
  return (
    $self->inc_mtimes && $self->built_mtime &&
    $self->inc_mtimes eq $inc_mtimes &&
    $self->built_mtime eq $built_mtime &&
    $self->fingerprint_calc_current
  ) ? 0 : 1;
}

sub prepare_asset {
  my ($self, $path) = @_;
  my $start = [gettimeofday];

  $self->built_file->touch unless (-e $self->built_file);

  # Special code path: if this is associated with a sub file request
  # in a 'directory' type asset, clear the asset to force a rebuild
  # below if the *subfile* mtime has changed
  $self->_subfile_mtime_verify($path) if ($self->is_dir && $path);

  # For 'directory' only consider the mtime of the top directory and don't
  # read in all the files (yet... we will read them in only if we need to rebuild)
  #  WARNING: this means that changes *within* sub files will not be detected here
  #  because that doesn't update the directory mtime; only filename changes will be seen.
  #  Update: That is what _subfile_mtime_verify above is for... to inexpensively catch
  #  this case for individual sub files
  my $files = $self->is_dir ? $self->includes : $self->get_include_files;
  my $inc_mtimes = $self->get_inc_mtime_concat($files);
  my $built_mtime = $self->get_built_mtime;

  # Check cached mtimes to see if anything has changed. This is a lighter
  # first pass check than the fingerprint check which calculates a sha1 for
  # all the source files and existing built files
  return 1 unless ( $self->_is_rebuild_required($inc_mtimes, $built_mtime) );

  ####  -----
  ####  The code above this line happens on every request and is designed
  ####  to be as fast as possible
  ####
  ####  The code below this line is (comparatively) expensive and only
  ####  happens when a rebuild is needed which should be rare--only when
  ####  content is modified, or on app startup (unless 'persist_state' is set)
  ####  -----

  ### Do a rebuild:

  # --- Blocks for up to 2 minutes waiting to get an exclusive lock or dies
  $self->get_build_lock;
  # ---

  if ($self->is_dir) {
    # Get the real list of files that we put off above for 'directory' assets
    $files = $self->get_include_files;
    # update the mtime cache of all directory subfiles
    $self->set_subfile_meta($files);
  }

  # Check the fingerprint to see if we can avoid a full rebuild (if mtimes changed
  # but the actual content hasn't by comparing the fingerprint/checksum):
  my $fingerprint = $self->calculate_fingerprint($files);
  my $cur_fingerprint = $self->current_fingerprint;
  if($fingerprint && $cur_fingerprint && $cur_fingerprint eq $fingerprint) {
    # If the mtimes changed but the fingerprint matches we don't need to regenerate. 
    # This will happen if another process just built the files while we were waiting 
    # for the lock and on the very first time after the application starts up
    $self->inc_mtimes($inc_mtimes);
    $self->built_mtime($built_mtime);
    $self->_persist_state;
    return $self->release_build_lock;
  }

  ### Ok, we really need to do a full rebuild:

  my $fd = file($self->built_file)->openw or die $!;
  if($self->is_dir) {
    # The built file is just a placeholder in the case of 'directory' type 
    # asset whose data is served from the original files
    my @relative = map { file($_)->relative($self->dir_root) } @$files;
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
  # we're calculating the fingerprint again because the built_file, which was just
  # regenerated, is included in the checksum data. This could probably be optimized,
  # however, this only happens on rebuild which rarely happens (should never happen)
  # in production so an extra second is no big deal in this case.
  $self->calculate_save_fingerprint($files);

  $self->_app->log->info(
    "Built asset: " . $self->asset_path .
    ' in ' . sprintf("%.3f", tv_interval($start) ) . 's'
   );

  # Release the lock and return:
  $self->_persist_state;
  return $self->release_build_lock;
}

sub file_checksum {
  my $self = shift;
  my $files = ref $_[0] eq 'ARRAY' ? $_[0] : \@_;
  
  my $Sha1 = Digest::SHA1->new;
  foreach my $file (@$files) {
    my $fh = file($file)->openr or die "$! : $file\n";
    $Sha1->addfile($fh);
    $fh->close;
  }
  
  return substr $Sha1->hexdigest, 0, $self->sha1_string_length;
}

sub asset_name {
  my $self = shift;
  my $sha1 = $self->current_fingerprint;
  return $self->is_dir ? $sha1 : $sha1 . '.' . $self->type;
  return  . '.' . $self->type;
}

# Provides a mechanism for preparing a set of subfiles all at once. This
# is a critical pre-step whenever multiple subfiles are being used together
# because if any have changed the asset path for *all* will be updated as
# soon as the changed file is detected. If this happens halfway through the list,
# the asset path of earlier processed items will retroactively change.
sub prepare_asset_subfiles {
  my ($self, @files) = @_;

  die "prepare_asset_subfiles() only applies to 'directory' assets"
    unless ($self->is_dir);

  $self->_subfile_mtime_verify($_) for (@files);
  $self->prepare_asset;
}

# this global is just used for some internal optimization to avoid calling stat
# duplicate times. It is basically me being lazy, adding an internal extra param
# to asset_path() without changing its public API/arg list
our $_ASSET_PATH_SKIP_PREPARE = 0;
sub asset_path {
  my ($self, @subpath) = @_;

  my $path = join('/',@subpath);
  $self->prepare_asset($path) unless ($_ASSET_PATH_SKIP_PREPARE);

  my $base = '/' . $self->action_namespace($self->_app) . '/' . $self->asset_name;
  return $base unless (scalar @subpath > 0);
  Catalyst::Exception->throw("Cannot use subpath with non directory asset")
    unless $self->is_dir;

  my $File = $self->dir_root->file(@subpath);
  Catalyst::Exception->throw("sub file '$path' not found") unless (-f $File);

  return join('/',$base,$path);
}


# These apply only to 'directory' asset type
has 'html_head_css_subfiles', is => 'ro', isa => 'ArrayRef', default => sub {[]};
has 'html_head_js_subfiles', is => 'ro', isa => 'ArrayRef', default => sub {[]};

# --------------------
# html_head_tags()
#
# Convenience method to generate a set of CSS <link> and JS <script> tags
# suitable to drop into the <head> section of an HTML document. 
#
# For 'css' and 'js' assets this will be a single tag pointing at the current
# valid asset path. For 'directory' asset types this will be a listing of
# css and/or js tags pointing at subfile asset paths supplied in the attrs:
# 'html_head_css_subfiles' and 'html_head_js_subfiles', or, supplied in a
#  hash(ref) argument with 'css' and/or 'js' keys and arrayref values.
#
# ### More about the 'directory' asset type:
#
# This could be considered a violation of separation of concerns, but the main
# reason this method is provided at all, besides the fact that it is a common
# use case, is that it handles the preprocessing required to ensure the dir asset
# is in an atomic/consistent state by calling prepare_asset_subfiles() on all
# supplied subfiles as a group to catch any content changes before rendering/returning
# the active asset paths. This is something that users might not realize they
# need to do if they don't read the docs closely. So, it is a common use case
# and this provides a simple and easy to understand interface that spares the user
# from needing to know about details they might not want to know about. It's
# practical/useful, self-documenting, and doesn't have to be used...
#
# The only actual "risk" if this the preprocessing step is missed, and the user builds
# head tags themselves with multiple calls to asset_path('path/to/subfile') [such as in
# a TT file] is that during a request where the content of one of the subfiles has changed,
# the asset paths of all the subfiles processed/returned prior to hitting the changed file
# will already be invalid (retroactively) because the sha1 will have changed. This is
# because the sha1/fingerprint is based on the asset as *whole*, and for performance, subfile
# content changes are not detected until they are accessed. This is only an issue when the
# content changes *in-place*, which shouldn't happen in a production environment. And, it
# only effects the first request immediately after the change. This issue can also be avoided
# altogether by using static 'current' alias redirect URLs instead off calling asset_path(),
# but this is *slightly* less efficient, as discussed in the documentation.
#
# This long-winded explanation is more about documenting/explaining the internal design
# for development purposes (and to be a reminder for me) than it is anything else. Also,
# it is intentionally in a comment rather than the POD for the sake of avoiding information
# overload since from the user perspective this is barely an issue (but very useful for
# developers who need to understand the internals of this module)
#
#  Note: This has nothing to do with 'css' or 'js' asset types which are always atomic
#  (because they are single files and have no "subfiles"). This *only* applies to
#  the 'directory' asset type
#
sub html_head_tags {
  my ($self, @args) = @_;
  
  my @tags = ();
  if($self->type eq 'css') {
    my $path = $self->asset_path;
    push @tags, '<link rel="stylesheet" type="text/css" href="' . $path . '" />';
  }
  elsif($self->type eq 'js') {
    my $path = $self->asset_path;
    push @tags, '<script type="text/javascript" src="' . $path . '"></script>';
  }
  elsif($self->type eq 'directory') {
    # get the files from either supplied arguments or defaults in object attrs:
    my %cnf = scalar @args > 0
      ? ( (ref($args[0]) eq 'HASH') ? %{ $args[0] } : @args ) # <-- arg as hash or hashref
      : ( css => $self->html_head_css_subfiles, js => $self->html_head_js_subfiles );
      
    # note that we're totally trusting the caller to know that these files are
    # in fact js/css files. We're just generating the correct tags for each type
    my @css = $cnf{css} ? @{$cnf{css}} : ();
    my @js = $cnf{js} ? @{$cnf{js}} : ();

    # This is the line that ensures any content changes are detected before we start
    # building the tags/urls:
    $self->prepare_asset_subfiles(@css,@js);

    # This spares repeating the stat/mtime calls by asset_path() below.
    # Maybe overkill, but every little bit of performance helps (and I'm OCD)...
    local $_ASSET_PATH_SKIP_PREPARE = 1;

    push @tags, '<link rel="stylesheet" type="text/css" href="' .
      $self->asset_path($_) . '" />' for (@css);

    push @tags, '<script type="text/javascript" src="' .
      $self->asset_path($_) . '"></script>' for (@js);
  }
  
  my $html =
		"<!--   AUTO GENERATED BY " . ref($self) . " (/" .
    $self->action_namespace($self->_app) . ")   -->\r\n" .
		( scalar @tags > 0 ?
			join("\r\n",@tags) : '<!--      NO ASSETS AVAILABLE      -->'
		) .
		"\r\n<!--  ---- END AUTO GENERATED ASSETS ----  -->\r\n";

  return $html;
}
# --------------------


sub asset_fh {
  my $self = shift;

  my $file = $self->built_file;
  return undef unless (-f $file);
  
  my $fh = file($file)->openr or die "$! : $file\n";
  return $fh;
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

# TODO: find a lib that does this with better cross-platform support. This
# is only known to work under Linux
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

1;

