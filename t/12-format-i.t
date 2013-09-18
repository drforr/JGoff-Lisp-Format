#!perl

use Test::More tests => 18;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

SKIP: {
  my $count = 16;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;

### pprint-indent.9
#(def-pprint-test format.i.1
#  (format nil "~<M~3:i~:@_M~:>" '(M M))
#  "M
#    M")

def_pprint_test 'format.i.1' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    '~<M~3:i~:@_M~:>',
    [ 'M', 'M' ]
  );
}, "M\n   M";

### See pprint-indent.10
#(def-pprint-test format.i.2
#  (format nil "~:<M~1:I~@:_M~:>" '(M M))
#  "(M
#   M)")

def_pprint_test 'format.i.2' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    '~<M~1:I~@:_M~:>',
    [ 'M', 'M' ]
  );
}, "(M\n   M)";

### See pprint-indent.11
#(def-pprint-test format.i.3
#  (format nil "~<(~;M~-1:i~:@_M~;)~:>" '(M M))
#  "(M
# M)")

def_pprint_test 'format.i.3' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    '~<(~;M~-1:i~:@_M~;)~:>',
    [ 'M', 'M' ]
  );
}, "(M\n M)";

#(def-pprint-test format.i.4
#  (format nil "~:<M~-1:i~:@_M~:>" '(M M))
#  "(M
# M)")

def_pprint_test 'format.i.4' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    '~:<M~-1:i~:@_M~:>',
    [ 'M', 'M' ]
  );
}, "(M\n M)";

#(def-pprint-test format.i.5
#  (format nil "~<(~;M~:I~:@_M~;)~:>" '(M M))
#  "(M
#  M)")

def_pprint_test 'format.i.5' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    '~<(~;M~:I~:@_M~;)~:>',
    [ 'M', 'M' ]
  );
}, "(M\n  M)";

#(def-pprint-test format.i.6
#  (format nil "~<(~;M~v:i~:@_M~;)~:>" '(nil))
#  "(M
#  M)")

def_pprint_test 'format.i.6' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    '~<(~;M~v:i~:@_M~;)~:>',
    [ undef ]
  );
}, "(M\n  M)";

#(def-pprint-test format.i.7
#  (format nil "~:<M~-2:i~:@_M~:>" '(M M))
#  "(M
#M)")

def_pprint_test 'format.i.7' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    '~:<M~-2:i~:@_M~:>',
    [ 'M', 'M' ]
  );
}, "(M\nM)";

#(def-pprint-test format.i.8
#  (format nil "~<M~:i~:@_M~:>" '(M M))
#  "M
# M")

def_pprint_test 'format.i.8' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    '~<M~:i~:@_M~:>',
    [ 'M', 'M' ]
  );
}, "(M\n M)";

### See pprint-indent.13
#(def-pprint-test format.i.9
#  (format nil "~<MMM~I~:@_MMMMM~:>" '(M M))
#  "MMM
#MMMMM")

def_pprint_test 'format.i.9' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    '~<MMM~I~:@_MMMMM~:>',
    [ 'M', 'M' ]
  );
}, "MMM\nMMMMM";

#(def-pprint-test format.i.10
#  (format nil "~:<MMM~I~:@_MMMMM~:>" '(M M))
#  "(MMM
# MMMMM)")

def_pprint_test 'format.i.10' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    '~:<MMM~I~:@_MMMMM~:>',
    [ 'M', 'M' ]
  );
}, "(MMM\nMMMMM)";

#(def-pprint-test format.i.11
#  (format nil "~<MMM~1I~:@_MMMMM~:>" '(M M))
#  "MMM
# MMMMM")

def_pprint_test 'format.i.11' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    '~<MMM~1I~:@_MMMMM~:>',
    [ 'M', 'M' ]
  );
}, "MMM\nMMMMM";

#(def-pprint-test format.i.12
#  (format nil "XXX~<MMM~1I~:@_MMMMM~:>" '(M M))
#  "XXXMMM
#    MMMMM")

def_pprint_test 'format.i.12' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    'XXX~<MMM~1I~:@_MMMMM~:>',
    [ 'M', 'M' ]
  );
}, "XXXMMM\n    MMMMM";

#(def-pprint-test format.i.13
#  (format nil "XXX~<MMM~I~:@_MMMMM~:>" '(M M))
#  "XXXMMM
#   MMMMM")

def_pprint_test 'format.i.13' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    'XXX~<MMM~I~:@_MMMMM~:>',
    [ 'M', 'M' ]
  );
}, "XXXMMM\n   MMMMM";

#(def-pprint-test format.i.14
#  (format nil "XXX~<MMM~-1I~:@_MMMMM~:>" '(M M))
#  "XXXMMM
#  MMMMM")

def_pprint_test 'format.i.14' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    'XXX~<MMM~-1I~:@_MMMMM~:>',
    [ 'M', 'M' ]
  );
}, "XXXMMM\n  MMMMM";

#(def-pprint-test format.i.15
#  (format nil "XXX~<MMM~vI~:@_MMMMM~:>" '(nil))
#  "XXXMMM
#   MMMMM")

def_pprint_test 'format.i.15' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    'XXX~<MMM~vI~:@_MMMMM~:>',
    [ undef ]
  );
}, "XXXMMM\n   MMMMM";

#(def-pprint-test format.i.16
#  (format nil "XXX~<MMM~vI~:@_MMMMM~:>" '(2))
#  "XXXMMM
#     MMMMM")

def_pprint_test 'format.i.16' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    'XXX~<MMM~vI~:@_MMMMM~:>',
    [ 2 ]
  );
}, "XXXMMM\n     MMMMM";

}
