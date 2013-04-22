Catalyst-Controller-AutoAssets
==============================

# NAME

Catalyst::Controller::AutoAssets - Automatic asset serving via sha1-based URLs

# VERSION

version 0.10

# SYNOPSIS

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
    



# DESCRIPTION

Fast, convenient serving of assets (css, javascript, etc) at URL path(s) containing a sha1 
checksum of the content. This is an alternative/supplement to [Catalyst::Plugin::Static::Simple](http://search.cpan.org/perldoc?Catalyst::Plugin::Static::Simple) or
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

# CONFIG PARAMS

## type

__Required__ - The asset type: `directory`, `css` or `js`.

The `directory` asset type works in a similar manner as Static::Simple to make some directory
structure accessible at a public URL. The root of the structure is made available at the URL path:

    <CONTROLLER_PATH>/<SHA1>/

[MIME::Types](http://search.cpan.org/perldoc?MIME::Types) is used to set the `Content-Type` HTTP header based on
the file extension (same as Static::Simple does).

Because the sha1 checksum changes automatically and is unknown in advance, the above Asset Path is made available
via the `asset_path()` controller method for use in TT files and throughout the application.

The `css` and `js` types serve one automatically generated text file that is concatenated and
optionally minified from the include files. The single, generated file is made available at the URL 
Path:

    <CONTROLLER_PATH>/<SHA1>.js    # for 'js' type
    <CONTROLLER_PATH>/<SHA1>.css   # for 'css' type

The js/css types provide a bonus mode of operation to provide a simple and convenient way to 
manage groups of CSS and JavaScript files to be automatically deployed in the application. This
is also particularly useful during development. Production applications with their own management
and build process for CSS and JavaScript would simply use the `directory` type.

## include

__Required__ - String or ArrayRef. The path(s) on the local filesystem containing the source asset files. 
For `directory` type this must be exactly one directory, while for `css` and `js` it can
be a list of directories. The `include` directory becomes the root of the files hosted as-is
for the `directory` type, while for `css` and `js` asset types it is the include files 
concatinated together (and possibly minified) to be served as the single file.

## current\_redirect

Whether or not to make the current asset available via 307 redirect to the
real, current checksum/fingerprint asset path. This is a pure HTTP mechanism of resolving the
asset path.

    <CONTROLLER_PATH>/current/      # for 'directory' type
    <CONTROLLER_PATH>/current.js    # for 'js' type
    <CONTROLLER_PATH>/current.css   # for 'css' type

For instance, you might reference a CSS file from a `directory` asset `Controller::Assets::ExtJS` 
using this URL path (i.e. href in an HTML `link` tag):

    /assets/extjs/current/resources/css/ext-all.css

This path would redirect (HTTP 307) to the current asset/file path which would be something like:

    /assets/extjs/1512834162611db1fab246dfa87e3a37f68ed95f/resources/css/ext-all.css

The downside of this is that the server has to serve the non-cachable redirect every time, which 
partially defeats the performance benefits of this module (although the redirect is comparatively lightweight).

The other mechanism to find the current asset path is via the `asset_path()` method, which returns
the current path outright and is the recommended usage, but is only available in locations where 
application controller methods can be called (like in TT files).

Defaults to true (1).

## current\_alias

Alias to use for the `current_redirect`. Defaults to 'current' (which also implies 'current.js'/'current.css'
for `js` and `css` asset types).

## minify

Whether or not to attempt to minify content for `css` or `js` asset types. This is a purely optional
convenience feature.

Defaults to false (0). Does not apply to the `directory` asset type.

## minifier

CodeRef used to minify the content when `minify` is true. The default code is a pass-through to 
`CSS::Minifier::minify()` for `css` assets and `JavaScript::Minifier::minify()` for `js`. If
you want to override you must follow the same API as in those modules, using the `input` and 
`outfile` filehandle interface. See [JavaScript::Minifier](http://search.cpan.org/perldoc?JavaScript::Minifier) and [CSS::Minifier](http://search.cpan.org/perldoc?CSS::Minifier) for more details.

Does not apply to the `directory` asset type.

## work\_dir

The directory where asset-specific files are generated and stored. This contains the checksum/fingerprint 
file, the lock file, and the built file. In the case of `directory` assets the built file contains a manifest
of files and in the case of `css` and `js` assets it contains the actual asset content (concatenated and 
possibly minified)

Defaults to:

    <APP_TMPDIR>/AutoAssets/<CONTROLLER_PATH>/

## max\_lock\_wait

Number of seconds to wait to obtain an exclusive lock when recalculating/regenerating. For thread-safety, when the system
needs to regenerate the asset (fingerprint and built file) it obtains an exclusive lock on the lockfile in the 
work\_dir. If another thread/process already has a lock, the system will wait for up to `max_lock_wait` seconds
before giving up and throwing an exception.

Note that this is only relevant when the source/include content changes while the app is running (which should never 
happen in a production environment).

Defaults to 120 seconds.

## max\_fingerprint\_calc\_age

Max number of seconds before recalculating the fingerprint of the content (sha1 checksum)
regardless of whether or not the mtime has changed. 0 means infinite/disabled.

For performance, once the system has calculated the checksum of the asset content it caches the mtime
of the include file(s) and verifies on each request to see if they have changed. If they have, it 
regenerates the asset on the fly (recalculates the checksum and concatenates and minifies (if enabled)
for `css` and `js` asset types). If `max_fingerprint_calc_age` is set to a non-zero value, it will force the
system to regenerate at least every N seconds regardless of the mtime. This would only be needed in cases
where you are worried the content could change without changing the mtime which shouldn't be needed in
most cases.

Defaults to 0.

## asset\_content\_type

The content type returned in the 'Content-Type' header. Defaults to `text/css` or `text/javascript`
for the `css` and `js` types respectively. 

Does not apply to `directory` asset type. For files within `directory` type assets, the Content-Type 
is set according to the file extension using [MIME::Types](http://search.cpan.org/perldoc?MIME::Types).

## cache\_control\_header

The HTTP `'Cache-Control'` header to return when serving assets. Defaults to the maximum 
aggressive value that should be honored by most browsers (1 year):

    public, max-age=31536000, s-max-age=31536000

# METHODS

## is\_dir

Returns true (1) if the asset type is `directory` and false (0) if the type is `css` or `js`.

## asset\_path

Returns the current, public URL path to the asset:

    <CONTROLLER_PATH>/<SHA1>       # for 'directory' type
    <CONTROLLER_PATH>/<SHA1>.js    # for 'js' type
    <CONTROLLER_PATH>/<SHA1>.css   # for 'css' type

For `directory` asset types, accepts an optional subpath argument to a specific file. For example,
if there was a file `images/logo.gif` within the include directory, $c->controller('Foo::MyAsset')->asset\_path('images/logo.gif')
might return:

    /foo/myasset/1512834162611d99fab246dfa87345a37f68ed95f/images/logo.gif

# SEE ALSO

- [Catalyst::Plugin::Assets](http://search.cpan.org/perldoc?Catalyst::Plugin::Assets)
- [Catalyst::Controller::VersionedURI](http://search.cpan.org/perldoc?Catalyst::Controller::VersionedURI)
- [Plack::Middleware::Assets](http://search.cpan.org/perldoc?Plack::Middleware::Assets)
- [Plack::Middleware::JSConcat](http://search.cpan.org/perldoc?Plack::Middleware::JSConcat)


# AUTHOR

Henry Van Styn <vanstyn@cpan.org>

# COPYRIGHT AND LICENSE

This software is copyright (c) 2013 by IntelliTree Solutions llc.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.
