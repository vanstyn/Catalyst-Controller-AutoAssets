package Catalyst::Controller::AutoAssets;
use strict;
use warnings;

# VERSION
# ABSTRACT: Controller for serving checksum-based assets

use Moose;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller' }

use Fcntl qw( :DEFAULT :flock :seek F_GETFL );
use Digest::SHA1;
use Path::Class qw( dir file );
use File::Spec;
use File::stat qw(stat);
use Catalyst::Utils;
use IO::File;
use IO::All;

require Module::Runtime;

my @valid_types = qw(js css directory);

has 'include', is => 'ro', isa => 'Str|ArrayRef[Str]', required => 1;  
has 'type', is => 'ro', isa => 'Str', required => 1;
has 'minify', is => 'ro', isa => 'Bool', default => sub{0};
has 'current_redirect', is => 'ro', isa => 'Bool', default => sub{1};


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
  
  $self->prepare_asset;
}

#############################
sub index :Path {
  my ( $self, $c, $arg ) = @_;
  
  $self->prepare_asset;
  
  return $c->detach('current_asset_request') if (
    $self->current_redirect &&
    ($arg eq 'current' || $arg eq 'current.' . $self->type)
  );
  
  return $self->is_dir ?
    $c->detach('directory_asset_request') :
    $c->detach('file_asset_request');
}

sub file_asset_request :Private {
  my ( $self, $c, @args ) = @_;
  
  my $want_asset = join('/',@args);

  my $asset = $self->asset_name;
  return $self->unknown_asset($c,$want_asset) unless ($asset eq $want_asset);
  
  # Let browsers cache forever because we're a CAS path! content will always be current
  $c->response->header(
    'Content-Type' => $self->asset_content_type,
    'Cache-Control' => 'public, max-age=31536000, s-max-age=31536000' # 31536000 = 1 year
  ); 
  
  return $c->response->body( $self->asset_fh );
}

sub directory_asset_request :Private { ... }

sub current_asset_request :Private {
  my ( $self, $c, $arg, @args ) = @_;
  
  $c->response->header( 'Cache-Control' => 'no-cache' );
  $c->response->redirect('/' . join('/',$self->asset_path,@args), 307);
  return $c->detach;
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

has 'work_dir', is => 'ro', lazy => 1, default => sub {
  my $self = shift;
  my $c = $self->_app;
  
  my $tmpdir = Catalyst::Utils::class2tempdir($c)
    || Catalyst::Exception->throw("Can't determine tempdir for $c");
    
  my $dir = dir($tmpdir, "AutoAssets",  $self->action_namespace($c));
  $dir->mkpath($c->debug) unless (-d $dir);
  return $dir->resolve;
};

has 'built_file', is => 'ro', lazy => 1, default => sub {
  my $self = shift;
  my $filename = 'built.' . $self->type;
  return File::Spec->catfile($self->work_dir,$filename);
};

has 'fingerprint_file', is => 'ro', lazy => 1, default => sub {
  my $self = shift;
  return File::Spec->catfile($self->work_dir,'fingerprint');
};

has 'lock_file', is => 'ro', lazy => 1, default => sub {
  my $self = shift;
  return File::Spec->catfile($self->work_dir,'lockfile');
};

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
      dir($inc)->recurse( callback => sub {
        my $child = shift;
        push @files, $child->absolute unless ($child->is_dir);
      });
    }
    else {
      Catalyst::Exception->throw("AutoAsset include path '$inc' not found");
    }
  }
    
  return \@files;
}


has 'max_fingerprint_calc_age', is => 'ro', isa => 'Int', default => sub{(60*60*12)}; # 12 hours
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
  my $fingerprint = io($self->fingerprint_file)->slurp;
  return $fingerprint;
}

sub save_fingerprint {
  my $self = shift;
  my $fingerprint = shift or die "Expected fingerprint/checksum argument";
  return io($self->fingerprint_file)->print($fingerprint);
}

sub calculate_save_fingerprint {
  my $self = shift;
  my $fingerprint = $self->calculate_fingerprint or return 0;
  return $self->save_fingerprint($fingerprint);
}

sub fingerprint_calc_current {
  my $self = shift;
  my $last = $self->last_fingerprint_calculated or return 0;
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
  
  my $fd = IO::File->new($self->built_file, '>:raw') or die $!;
  if($self->is_dir) {
    # The built file is just a placeholder in the case of 'directory' type 
    # asset whose data is served from the original files
    $fd->write(join("\r\n",$inc_mtimes,@$files) . "\r\n");
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
      $fd->write($_) for ( map { io($_)->slurp . "\r\n" } @$files );
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
    my $FH = IO::File->new();
    $FH->open('< ' . $file) or die "$! : $file\n";
    $FH->binmode;
    $Sha1->addfile($FH);
    $FH->close;
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
  my $self = shift;
  return $self->action_namespace($self->_app) . '/' . $self->asset_name;
}

sub asset_content_type {
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
}


sub asset_fh {
  my $self = shift;

  my $file = $self->built_file;
  return undef unless (-f $file);
  
  my $fh = IO::File->new();
  $fh->open('< ' . $file) or die "Failed to open $file for reading.";
  
  return $fh;
}

sub unknown_asset {
  my ($self,$c, $asset) = @_;
  $c->res->status(404);
  return $c->res->body("No such asset '$asset'");
}

has 'max_lock_wait', is => 'ro', isa => 'Int', default => 120;

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
