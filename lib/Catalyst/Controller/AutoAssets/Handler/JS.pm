package Catalyst::Controller::AutoAssets::Handler::JS;
use strict;
use warnings;

# VERSION

use Moose;
use namespace::autoclean;

extends 'Catalyst::Controller::AutoAssets::Handler::CSS';

use Module::Runtime;

has 'minifier', is => 'ro', isa => 'Maybe[CodeRef]', lazy => 1, default => sub {
  my $self = shift;
  Module::Runtime::require_module('JavaScript::Minifier');
  return sub { JavaScript::Minifier::minify(@_) };
};

has 'asset_content_type', is => 'ro', isa => 'Str', default => 'text/javascript';
has 'ext', is => 'ro', isa => 'Str', default => 'js';

sub html_head_tags {
  my $self = shift;
  return
		"<!--   AUTO GENERATED BY " . ref($self->_Controller) . " (/" .
    $self->action_namespace($self->_app) . ")   -->\r\n" .
		'<script type="text/javascript" src="' . 
    $self->asset_path .
    '"></script>' .
		"\r\n<!--  ---- END AUTO GENERATED ASSETS ----  -->\r\n";
}

1;

__END__

=pod

=head1 NAME

Catalyst::Controller::AutoAssets::Handler::JS - JS type handler

=head1 DESCRIPTION

This is the Handler class for the 'JS' asset type. This is a core type and is
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
