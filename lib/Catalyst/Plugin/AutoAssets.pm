package Catalyst::Plugin::AutoAssets;
use strict;
use warnings;

our $VERSION = 0.11;

use Moose::Role;
use namespace::autoclean;

use CatalystX::InjectComponent;
use Catalyst::Controller::AutoAssets;

before 'setup_components' => sub { (shift)->inject_asset_controllers(@_) };

sub inject_asset_controllers {
  my $c = shift;

  my $config = $c->config->{'Plugin::AutoAssets'} or return;
  my $assets = $config->{assets} or die "No 'assets' defined in 'Plugin::AutoAssets' config!";

  # Apply the Controller configs first:

  my @controllers = ();
  if (ref $assets eq 'HASH') {
    $c->config( "Controller::$_" => $assets->{$_} ) and push @controllers, $_ 
      for (keys %$assets);
  }
  elsif (ref $assets eq 'ARRAY') {
    my %seen = ();
    for my $orig (@$assets) {
      my $cnf = { %$orig };
      my $controller = delete $cnf->{controller} 
        or die "Bad asset config; 'controller' class not specified";
      die "Duplicate asset controller '$controller'" if ($seen{$controller}++);
      $c->config( "Controller::$controller" => $cnf ) and push @controllers, $controller;
    }
  }
  else {
    die "'assets' must be a hashref or an arrayref";
  }

  # Now inject the new Controllers:
  CatalystX::InjectComponent->inject(
    into => $c,
    component => 'Catalyst::Controller::AutoAssets',
    as => $_
  ) for (@controllers);

  # Record the list in a class attr
  $c->mk_classdata('asset_controllers');
  $c->asset_controllers(\@controllers);
}



1;

__END__

=pod

=head1 NAME

Catalyst::Plugin::AutoAssets - Plugin interface to Catalyst::Controller::AutoAssets

=head1 VERSION

version 0.11

=head1 SYNOPSIS

  use Catalyst;
  with 'Catalyst::Plugin::AutoAssets';
  
  # Inject/setup AutoAssets controllers: 
  #  * MyApp::Controller::Assets::ExtJS   (/assets/extjs)
  #  * MyApp::Controller::Assets::MyCSS   (/assets/mycss)
  __PACKAGE__->config(
    name => 'MyApp',
    'Plugin::AutoAssets' => {
      assets => {
        'Assets::ExtJS' => {
          type => 'directory',
          include => 'ext-3.4.0',
          persist_state => 1,
          sha1_string_length => 15
        },
        'Assets::MyCSS' => {
          type => 'css',
          include => '/path/to/css',
          minify => 1
        }
      }
    }
  );
  
  # Or, using arrayref syntax (if order is important):
  __PACKAGE__->config(
    name => 'MyApp',
    'Plugin::AutoAssets' => {
      assets => [
        {
          controller => 'Assets::ExtJS',
          type => 'directory',
          include => 'ext-3.4.0',
          persist_state => 1,
          sha1_string_length => 15
        },
        {
          controller => 'Assets::MyCSS',
          type => 'css',
          include => '/path/to/css',
          minify => 1
        }
      ]
    }
  );


=head1 DESCRIPTION

This class provides a simple Catalyst Plugin interface to L<Catalyst::Controller::AutoAssets> for easy
setup of multiple AutoAssets controllers via config. To use, simply pass a hashref (or arrayref) of 'assets' into the 
config key 'Plugin::AutoAssets' in your Catalyst application config. This hash should contain controller 
class names in the keys and Catalyst::Controller::AutoAssets hash configs in the values. Each controller 
will be injected into your application at runtime.

This is just a faster setup than creating the controller classes manually. See L<Catalyst::Controller::AutoAssets>
for details and supported config params.

=head1 AUTHOR

Henry Van Styn <vanstyn@cpan.org>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2013 by IntelliTree Solutions llc.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
