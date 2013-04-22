# -*- perl -*-

use strict;
use warnings;
use FindBin '$Bin';
use lib "$Bin/lib";

my $work_dir = "$Bin/var/tmp/work_dir";

{
  package TestApp;
  use Moose;
  
  use Catalyst;
  extends 'Catalyst';
  
  __PACKAGE__->config(
    name => __PACKAGE__,
    'Controller::Assets' => {
      include => [ "$FindBin::Bin/var/eg_src/stylesheets" ],
      type => 'css',
      minify => 0,
      work_dir => $work_dir
    },
  );

  __PACKAGE__->setup();  
  1;
}

use Test::More;
use Catalyst::Test 'TestApp';

action_ok(
  '/assets/5fc09bfcb5ab7637fce05dbeec7ec1f2388aeccf.css',
  "Expected built asset SHA-1 path"
);

action_redirect(
  '/assets/current.css',
  "Current redirect"
);

action_ok(
  TestApp->controller('Assets')->asset_path,
  "Controller asset_path() method"
);

contenttype_is(
  '/assets/5fc09bfcb5ab7637fce05dbeec7ec1f2388aeccf.css',
  'text/css',
  "Expected CSS Content-Type"
);


action_notfound(
  '/assets/bad_asset_name',
  "Not found asset"
);

done_testing;