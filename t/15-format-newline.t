#!perl

use Test::More tests => 1;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

def_format_test 'format.newline.1' =>
  concatenate( "~", "\n", "   X" ),
  undef,
  "X";

def_format_test 'format.newline.2' =>
  concatenate( "A~:", "\n", " X" ),
  undef,
  "A X";

def_format_test 'format.newline.3' =>
  concatenate( "A~@", "\n", " X" ),
  undef,
  #.(concatenate 'string "A" (string #\Newline) "X");

