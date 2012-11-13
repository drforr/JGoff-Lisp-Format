#!perl

use Test::More tests => 1;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

def_format_test 'format.?.1' =>
  "~?",
  [ "", undef ],
  "";

def_format_test 'format.?.2' =>
  "~?",
  [ "~A", [ 1 ] ],
  "1";

def_format_test 'format.?.3' =>
  "~?",
  [ "", [ 1 ] ],
  "";

def_format_test 'format.?.4' =>
  "~? ~A",
  [ "", [ 1 ], 2 ],
  " 2";

def_format_test 'format.?.5' =>
  "a~?z",
  [ "b~?y", [ "c~?x", [ "~A", [ 1 ] ] ] ],
  "abc1xyz";

### Tests of ~@?

def_format_test 'format.@?.1' =>
  "~@?",
  [ "" ],
  "";

def_format_test 'format.@?.2' =>
  "~@?",
  [ "~A", 1 ],
  "1";

def_format_test 'format.@?.3' =>
  "~@? ~A",
  [ "<~A>", 1, 2 ],
  "<1> 2";

def_format_test 'format.@?.4' =>
  "a~@?z",
  [ "b~@?y", "c~@?x", "~A", 1 ],
  "abc1xyz";

def_format_test 'format.@?.5' =>
  "~{~A~@?~A~}",
  [ [ 1, "~4*", 2, 3, 4, 5, 6 ] ],
  "16";
