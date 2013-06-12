package Catalyst::Controller::AutoAssets;
use strict;
use warnings;

our $VERSION = 0.11;

use Moose;
use namespace::autoclean;
require Module::Runtime;

BEGIN { extends 'Catalyst::Controller' }

has 'type', is => 'ro', isa => 'Str', required => 1;

# Save the build params (passed to constructor)
has '_build_params', is => 'ro', isa => 'HashRef', required => 1;
around BUILDARGS => sub {
  my ($orig, $class, $c, @args) = @_;
  my %params = (ref($args[0]) eq 'HASH') ? %{ $args[0] } : @args; # <-- arg as hash or hashref
  $params{_build_params} = {%params};
  return $class->$orig($c,\%params);
};

# The Handler (which is determined by the asset type) is 
# where most of the actual work gets done:
has '_Handler' => (
  is => 'ro', init_arg => undef, lazy => 1,
  does => 'Catalyst::Controller::AutoAssets::Handler',
  handles => [qw(request asset_path html_head_tags release_build_lock)],
  default => sub {
    my $self = shift;
    my $class = $self->_resolve_handler_class($self->type);
    return $class->new({
      %{$self->_build_params},
      _Controller => $self
    });
  }
);

sub _resolve_handler_class {
	my $self = shift;
  my $class = shift;
  # built-in type names:
  my %type_aliases = ( css => 'CSS', js => 'JS', directory => 'Directory' );
  $class = $type_aliases{$class} if (exists $type_aliases{$class});
  
  # Allow absolute class names using '+' prefix:
  $class = $class =~ /^\+(.*)$/ ? $1 
    : "Catalyst::Controller::AutoAssets::Handler::$class";
	Module::Runtime::require_module($class);
	return $class;
}

sub BUILD {
  my $self = shift;
  
  # init type handler:
  $self->_Handler;
}

sub index :Path {
  my ($self, $c, @args) = @_;
  $self->request($c,@args);
  $c->detach;
}

sub end :Private {
  my ($self,$c) = @_;
  # Make sure we never keep a build lock past the end of a request:
  $self->release_build_lock;
}

sub unknown_asset {
  my ($self,$c,$asset) = @_;
  $asset ||= $c->req->path;
  $c->res->status(404);
  $c->res->header( 'Content-Type' => 'text/plain' );
  $c->res->body( "No such asset '$asset'" );
  $self->release_build_lock;
  return $c->detach;
}

1;

__END__

=pod

=head1 NAME

Catalyst::Controller::AutoAssets - Automatic asset serving via sha1-based URLs

=head1 VERSION

version 0.11

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

=head2 persist_state

Whether or not to persist and use state data (fingerprints and mtimes) across restarts to avoid rebuilding which may
be expensive and unnecessary. The asset fingerprint is normally always recalculated at startup, but if this option
is enabled it is loaded from a cache/state file maintained on disk. This is useful for assets that take a long time
to build (such as big include libs) and is fine as long as you trust the state data stored on disk.

WARNING: Use this feature with caution for 'directory' type assets since the mtime check does not catch file content changes
alone (only filename changes), and when this is enabled it may not catch changes even across app restarts which may
not be expected.

No effect if max_fingerprint_calc_age is set.

Defaults to false (0).

=head2 asset_content_type

The content type returned in the 'Content-Type' header. Defaults to C<text/css> or C<text/javascript>
for the C<css> and C<js> types respectively. 

Does not apply to C<directory> asset type. For files within C<directory> type assets, the Content-Type 
is set according to the file extension using L<MIME::Types>.

=head2 cache_control_header

The HTTP C<'Cache-Control'> header to return when serving assets. Defaults to the maximum 
aggressive value that should be honored by most browsers (1 year):

  public, max-age=31536000, s-max-age=31536000

=head2 sha1_string_length

Optional custom length (truncated) for the SHA1 fingerprint/checksum hex string. The full 40 characters is
probably overkill and so this option is provided if shorter URLs are desired. The lower the number the greater
the chance of collision, so you just need to balance the risk with how much you want shorter URLs (not that under normal
use cases these URLs need to be entered by a human in the first place). If you don't understand what this means then
just leave this setting alone.

Must be a integer between 5 and 40.

Defaults to 40 (full SHA1 hex string).

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

=head1 BUGS

Does not currently work on all Windows platforms because of the file locking code.
This will be refactored/generalized in a later version.

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
