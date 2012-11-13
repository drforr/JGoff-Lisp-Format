#!perl

use Test::More tests => 1;

BEGIN {
    use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!\n";
}

use strict;
use warnings;

my $f = JGoff::Lisp::Format->new;

