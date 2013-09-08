#!perl

use Test::More tests => 85;
use List::Util qw( min );

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

def_format_test 'format.a.1' =>
  "~a",
  [ undef ],
  "UNDEF";

deftest 'format.a.2' => sub {
  my $f = JGoff::Lisp::Format->new;
  with_standard_io_syntax {
    # XXX strictly speaking, *print-case* should be somewhere in core perl.
    # XXX but since it doesn't exist, make one up.
    # XXX
    local $JGoff::Lisp::Format::print_case = $JGoff::Lisp::Format::downcase;
    $f->format( undef, "~A", undef );
  };
}, "undef";

deftest 'formatter.a.2' => sub {
  my $f = JGoff::Lisp::Format->new;
  with_standard_io_syntax {
    local $JGoff::Lisp::Format::print_case = $JGoff::Lisp::Format::downcase;
    formatter_call_to_string(
      $f->formatter( "~A" ),
      undef
    );
  };
}, "undef";

deftest 'format.a.3' => sub {
  my $f = JGoff::Lisp::Format->new;
  with_standard_io_syntax {
    local $JGoff::Lisp::Format::print_case = $JGoff::Lisp::Format::capitalize;
    $f->format( undef, "~a", undef );
  };
}, "Undef";

deftest 'formatter.a.3' => sub {
  my $f = JGoff::Lisp::Format->new;
  with_standard_io_syntax {
    local $JGoff::Lisp::Format::print_case = $JGoff::Lisp::Format::capitalize;
    formatter_call_to_string(
      $f->formatter( "~a" ),
      undef
    );
  };
}, "Undef";

def_format_test 'format.a.4' =>
  "~:a",
  [ undef ],
  "[]";

def_format_test 'format.a.5' =>
  "~:A",
  [ [ undef ] ],
  "[UNDEF]";

#def_format_test 'format.a.6' => "~:A", [ [ undef ] ], "[UNDEF]";
#  "~:A" (#(nil)) "#(NIL)") # Perl doesn't really have the notion of symbols

deftest 'format.a.7' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~a" );
  my $list = [];
  for my $c ( @JGoff::Lisp::Format::Utils::standard_chars ) {
    my $s1 = string( $c );
    my $s2 = $f->format( undef, "~a", $s1 );
    my $s3 = formatter_call_to_string( $fn, $s1 );
    unless ( $s1 eq $s2 and $s2 eq $s3 ) {
      collect( $list, $c, $s1, $s2, $s3 );
    }
  };
  return $list;
}, [];

#(deftest format.a.8
#  (let ((fn (formatter "~A")))
#    (loop with count = 0
#          for i from 0 below (min #x10000 char-code-limit)
#          for c = (code-char i)
#          for s1 = (and c (string c))
#          for s2 = (and c (format nil "~A" s1))
#          for s3 = (and c (formatter-call-to-string fn s1))
#          unless (or (null c) (string= s1 s2) (string= s2 s3))
#          do (incf count) and collect (list c s1 s2 s3)
#          when (> count 100) collect "count limit exceeded" and do (loop-finish)))
#  nil)

#(deftest format.a.9
#  (with-standard-io-syntax
#   (apply
#    #'values
#    (loop for i from 1 to 10
#          for fmt = (format nil "~~~d@a" i)
#          for s = (format nil fmt nil)
#          for fn = (eval `(formatter ,fmt))
#          for s2 = (formatter-call-to-string fn nil)
#          do (assert (string= s s2))
#          collect s)))
#  "NIL"
#  "NIL"
#  "NIL"
#  " NIL"
#  "  NIL"
#  "   NIL"
#  "    NIL"
#  "     NIL"
#  "      NIL"
#  "       NIL")

#(deftest format.a.10
#  (with-standard-io-syntax
#   (apply
#    #'values
#    (loop for i from 1 to 10
#          for fmt = (format nil "~~~da" i)
#          for s = (format nil fmt nil)
#          for fn = (eval `(formatter ,fmt))
#          for s2 = (formatter-call-to-string fn nil)
#          do (assert (string= s s2))
#          collect s)))
#  "NIL"
#  "NIL"
#  "NIL"
#  "NIL "
#  "NIL  "
#  "NIL   "
#  "NIL    "
#  "NIL     "
#  "NIL      "
#  "NIL       ")

#(deftest format.a.11
#  (with-standard-io-syntax
#   (apply
#    #'values
#    (loop for i from 1 to 10
#          for fmt = (format nil "~~~d@:A" i)
#          for s = (format nil fmt nil)
#          for fn = (eval `(formatter ,fmt))
#          for s2 = (formatter-call-to-string fn nil)
#          do (assert (string= s s2))
#          collect s)))
#  "()"
#  "()"
#  " ()"
#  "  ()"
#  "   ()"
#  "    ()"
#  "     ()"
#  "      ()"
#  "       ()"
#  "        ()")

#(deftest format.a.12
#  (with-standard-io-syntax
#   (apply
#    #'values
#    (loop for i from 1 to 10
#          for fmt = (format nil "~~~d:a" i)
#          for s = (format nil fmt nil)
#          for fn = (eval `(formatter ,fmt))
#          for s2 = (formatter-call-to-string fn nil)
#          do (assert (string= s s2))
#          collect s)))
#  "()"
#  "()"
#  "() "
#  "()  "
#  "()   "
#  "()    "
#  "()     "
#  "()      "
#  "()       "
#  "()        ")

#(deftest format.a.13
#  (with-standard-io-syntax
#   (apply
#    #'values
#    (let ((fn (formatter "~V:a")))
#      (loop for i from 1 to 10
#            for s = (format nil "~v:A" i nil)
#            for s2 = (formatter-call-to-string fn i nil)
#            do (assert (string= s s2))
#            collect s))))
#  "()"
#  "()"
#  "() "
#  "()  "
#  "()   "
#  "()    "
#  "()     "
#  "()      "
#  "()       "
#  "()        ")

#(deftest format.a.14
#  (with-standard-io-syntax
#   (apply
#    #'values
#    (let ((fn (formatter "~V@:A")))
#      (loop for i from 1 to 10
#            for s = (format nil "~v:@a" i nil)
#            for s2 = (formatter-call-to-string fn i nil)
#            do (assert (string= s s2))
#            collect s))))
#  "()"
#  "()"
#  " ()"
#  "  ()"
#  "   ()"
#  "    ()"
#  "     ()"
#  "      ()"
#  "       ()"
#  "        ()")

### With padchar

def_format_test 'format.a.15' =>
  "~va",
  [ undef, undef ],
  "UNDEF";

def_format_test 'format.a.16' =>
  "~v:A",
  [ undef, undef ],
  "[]";

def_format_test 'format.a.17' =>
  '~@a',
  [ undef ],
  "UNDEF";

def_format_test 'format.a.18' =>
  '~v@A',
  [ undef, undef ],
  "UNDEF";

def_format_test 'format.a.19' =>
  '~v:@a',
  [ undef, undef ],
  "[]";

def_format_test 'format.a.20' =>
  '~v@:a',
  [ undef, undef ],
  "[]";

### With colinc specified

#
# I've changed these formatting tests from the original ANSI spec to reflect
# the nature of the string we're checking against.
#
# The first number is the padded length, and the second one the number of
# characters at a time we have to work with.
#
# So, for instance, padding 'nil' to 3,1 means padding out to 3 characters,
# which is trivially done by not padding.
#
# The natural equivalent in perl is 'undef', so testing to 5,1 reflects the
# spirit of the original test.

def_format_test 'format.a.21' =>
  "~5,1a",
  [ undef ],
  "UNDEF";

def_format_test 'format.a.22' =>
  "~6,5a",
  [ undef ],
  "UNDEF     ";

def_format_test 'format.a.23' =>
  '~5,5@a',
  [ undef ],
  "UNDEF";

def_format_test 'format.a.24' =>
  '~6,6@a',
  [ undef ],
  "      UNDEF";

def_format_test 'format.a.25' =>
  '~9,5@a',
  [ undef ],
  "     UNDEF";

def_format_test 'format.a.26' =>
  "~9,5A",
  [ undef ],
  "UNDEF     ";

def_format_test 'format.a.27' =>
  '~11,5@a',
  [ undef ],
  "          UNDEF";

def_format_test 'format.a.28' =>
  "~11,5A",
  [ undef ],
  "UNDEF          ";

deftest 'format.a.29' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~v,,2A" );
  my $list = [];
  for my $i ( -4 .. 10 ) {
    my $s = $f->format( undef, "~v,,2A", $i, "ABC" );
    my $s2 = formatter_call_to_string( $fn, $i, "ABC" );
    is( $s, $s2, 'format.a.29' ); # XXX capture the name.
    push @$list, $s; # XXX Different (collect)
  };
  return $list;
}, [
  "ABC  ", "ABC  ", "ABC  ",
  "ABC  ",
  "ABC  ",
  "ABC  ",
  "ABC  ",
  "ABC  ",
  "ABC  ",
  "ABC  ",
  "ABC   ",
  "ABC    ",
  "ABC     ",
  "ABC      ",
  "ABC       "
];

### With padchar

def_format_test 'format.a.30' =>
  "~3,,+2A",
  [ "ABC" ],
  "ABC  ";

def_format_test 'format.a.31' =>
  "~3,,0A",
  [ "ABC" ],
  "ABC";

def_format_test 'format.a.32' =>
  "~3,,-1A",
  [ "ABC" ],
  "ABC";

def_format_test 'format.a.33' =>
  "~3,,0A",
  [ "ABCD" ],
  "ABCD";

def_format_test 'format.a.34' =>
  "~3,,-1A",
  [ "ABCD" ],
  "ABCD";

### With padchar

def_format_test 'format.a.35' =>
  "~4,,,'XA",
  [ "AB" ],
  "ABXX";

def_format_test 'format.a.36' =>
  "~4,,,a",
  [ "AB" ],
  "AB  ";

def_format_test 'format.a.37' =>
  q{~4,,,'X@a},
  [ "AB" ],
  "XXAB";

def_format_test 'format.a.38' =>
  '~4,,,@A',
  [ "AB" ],
  "  AB";

def_format_test 'format.a.39' =>
  "~10,,,vA",
  [ undef, "abcde" ],
  "abcde     ";

def_format_test 'format.a.40' =>
  '~10,,,v@A',
  [ undef, "abcde" ],
  "     abcde";

def_format_test 'format.a.41' =>
  "~10,,,va",
  [ '*', "abcde" ],
  "abcde*****";

def_format_test 'format.a.42' =>
  '~10,,,v@a',
  [ '*', "abcde" ],
  "*****abcde";

### Other tests

def_format_test 'format.a.43' =>
  "~3,,vA",
  [ undef, "ABC" ],
  "ABC";

def_format_test 'format.a.45' =>
  "~4,,va",
  [ -1, "abcd" ],
  "abcd";

deftest 'format.a.44' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~3,,vA" );
  my $list = [];
  for my $i ( 0 .. 6 ) {
    my $s = $f->format( undef, "~3,,vA", $i, 'ABC' );
    my $s2 = formatter_call_to_string( $fn, $i, 'ABC' );
    is( $s, $s2, 'format.a.44' );
    push @$list, $s;
  }
  return $list;
}, [
  "ABC",
  "ABC ",
  "ABC  ",
  "ABC   ",
  "ABC    ",
  "ABC     ",
  "ABC      "
];

deftest 'format.a.44a' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( '~3,,v@A' );
  my $list = [];
  for my $i ( 0 .. 6 ) {
    my $s = $f->format( undef, '~3,,v@A', $i, 'ABC' );
    my $s2 = formatter_call_to_string( $fn, $i, 'ABC' );
    is( $s, $s2, 'format.a.44' );
    push @$list, $s;
  }
  return $list;
}, [
  "ABC",
  " ABC",
  "  ABC",
  "   ABC",
  "    ABC",
  "     ABC",
  "      ABC"
];

def_format_test 'format.a.46' =>
  "~5,vA",
  [ undef, "abc" ],
  "abc  ";

def_format_test 'format.a.47' =>
  "~5,vA",
  [ 3, "abc" ],
  "abc   ";

def_format_test 'format.a.48' =>
  '~5,v@A',
  [ 3, "abc" ],
  "   abc";

### # parameters

def_format_test 'format.a.49' =>
  "~#A",
  [ "abc", undef, undef, undef ],
  "abc ",
  3;

def_format_test 'format.a.50' =>
  '~#@a',
  [ "abc", undef, undef, undef, undef, undef ],
  "   abc",
  5;

def_format_test 'format.a.51' =>
  "~5,#a",
  [ "abc", undef, undef, undef ],
  "abc    ", 3;

def_format_test 'format.a.52' =>
  '~5,#@A',
  [ "abc", undef, undef, undef ],
  "    abc", 3;

def_format_test 'format.a.53' =>
  "~4,#A",
  [ "abc", undef, undef ],
  "abc   ",
  2;

def_format_test 'format.a.54' =>
  '~4,#@A',
  [ "abc", undef, undef ],
  "   abc",
  2;

def_format_test 'format.a.55' =>
  "~#,#A",
  [ "abc", undef, undef, undef ],
  "abc    ",
  3;

def_format_test 'format.a.56' =>
  '~#,#@A',
  [ "abc", undef, undef, undef ],
  "    abc",
  3;

def_format_test 'format.a.57' =>
  "~-100A",
  [ "xyz" ],
  "xyz";

def_format_test 'format.a.58' =>
  "~-100000000000000000000a",
  [ "xyz" ],
  "xyz";

def_format_test 'format.a.jgoff.1' =>
  "~A ~A",
  [ "xyz", "abc" ],
  "xyz abc";
