package Catalyst::Controller::AutoAssets::Handler::CSS;
use strict;
use warnings;

# VERSION

use Moose;
use namespace::autoclean;

with 'Catalyst::Controller::AutoAssets::Handler';

use Path::Class 0.32 qw( dir file );
use Module::Runtime;

has 'minify', is => 'ro', isa => 'Bool', default => sub{0};

sub BUILD {
  my $self = shift;
  
  Catalyst::Exception->throw("No minifier available")
    if($self->minify && ! $self->minifier);
}

has 'minifier', is => 'ro', isa => 'Maybe[CodeRef]', lazy => 1, default => sub {
  my $self = shift;
  Module::Runtime::require_module('CSS::Minifier');
  return sub { CSS::Minifier::minify(@_) };
};

has 'asset_content_type', is => 'ro', isa => 'Str', default => 'text/css';
has 'ext', is => 'ro', isa => 'Str', default => 'css';

sub is_current_request_arg {
  my ($self, $arg) = @_;
  return (
    $arg eq $self->current_alias ||
    $arg eq $self->current_alias . '.' . $self->ext
  ) ? 1 : 0;
}

sub asset_request {
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

sub asset_name {
  my $self = shift;
  return $self->current_fingerprint . '.' . $self->ext;
}

sub asset_fh {
  my $self = shift;

  my $file = $self->built_file;
  return undef unless (-f $file);
  
  my $fh = file($file)->openr or die "$! : $file\n";
  return $fh;
}

sub html_head_tags {
  my $self = shift;
  return
		"<!--   AUTO GENERATED BY " . ref($self->_Controller) . " (/" .
    $self->action_namespace($self->_app) . ")   -->\r\n" .
		'<link rel="stylesheet" type="text/css" href="' . 
    $self->asset_path .
    '" />' .
		"\r\n<!--  ---- END AUTO GENERATED ASSETS ----  -->\r\n";
}

sub write_built_file {
  my ($self, $fd, $files) = @_;
  
  if($self->minify && $self->minifier) {
    foreach my $file (@$files) {
      open(INFILE, $file) or die $!;
      $self->minifier->( input => *INFILE, outfile => $fd );
      close INFILE;
      $fd->write("\r\n");
    }
  }
  else {
    $fd->write($_) for ( map { $_->slurp . "\r\n" } @$files );
  }
}

1;

__END__

=pod

=head1 NAME

Catalyst::Controller::AutoAssets::Handler::CSS - CSS type handler

=head1 DESCRIPTION

This is the Handler class for the 'CSS' asset type. This is a core type and is
documented in L<Catalyst::Controller::AutoAssets>.

=head1 SEE ALSO

=over

=item L<Catalyst::Controller::AutoAssets::Handler>

=back

=head1 AUTHOR

Henry Van Styn <vanstyn@cpan.org>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2013 by IntelliTree Solutions llc.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut

