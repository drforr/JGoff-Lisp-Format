#!perl -T

use Test::More tests => 1;

BEGIN {
    use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!\n";
}

diag( "Testing JGoff::Lisp::Format $JGoff::Lisp::Format::VERSION, Perl $], $^X" );
