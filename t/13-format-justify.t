#!perl

use Test::More tests => 44;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

SKIP: {
  my $count = 42;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

#(def-pprint-test format.justify.1
#  (format nil "~<~>")
#  "")

def_pprint_test 'format.justify.1' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format( undef, "~<~>" );
}, '';

#(def-pprint-test format.justify.2
#  (loop for i from 1 to 20
#        for s1 = (make-string i :initial-element #\x)
#        for s2 = (format nil "~<~A~>" s1)
#        unless (string= s1 s2)
#        collect (list i s1 s2))
#  nil)

def_pprint_test 'format.justify.2' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  my $collector = _make_collector( $remainder );
  for my $i ( 1 .. 20 ) {
    my $s1 = make_string( $i, initial_element => "x" );
    my $s2 = $f->format( undef, "~<~A~>", $s1 );
    unless ( $s1 eq $s2 ) {
      $collector->( list( $i, $s1, $s2 ) );
    }
  }
  return $remainder;
}, [];

#(def-pprint-test format.justify.3
#  (loop for i from 1 to 20
#        for s1 = (make-string i :initial-element #\x)
#        for s2 = (format nil "~<~A~;~A~>" s1 s1)
#        unless (string= s2 (concatenate 'string s1 s1))
#        collect (list i s1 s2))
#  nil)

def_pprint_test 'format.justify.3' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  my $collector = _make_collector( $remainder );
  for my $i ( 1 .. 20 ) {
    my $s1 = make_string( $i, initial_element => "x" );
    my $s2 = $f->format( undef, "~<~A~;~A~>", $s1, $s1 );
    unless ( $s2 eq concatenate( $s1, $s1 ) ) {
      $collector->( list( $i, $s1, $s2 ) );
    }
  }
  return $remainder;
}, [];

#(def-pprint-test format.justify.4
#  (loop for i from 1 to 20
#        for s1 = (make-string i :initial-element #\x)
#        for expected = (concatenate 'string s1 " " s1)
#        for s2 = (format nil "~,,1<~A~;~A~>" s1 s1)
#        unless (string= s2 expected)
#        collect (list i expected s2))
#  nil)

def_pprint_test 'format.justify.4' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  my $collector = _make_collector( $remainder );
  for my $i ( 1 .. 20 ) {
    my $s1 = make_string( $i, initial_element => "x" );
    my $expected = concatenate( $s1, ' ', $s1 );
    my $s2 = $f->format( undef, "~,,1<~A~;~A~>", $s1, $s1 );
    unless ( $s2 eq $expected ) {
      $collector->( list( $i, $expected, $s2 ) );
    }
  }
  return $remainder;
}, [];

#(def-pprint-test format.justify.5
#  (loop for i from 1 to 20
#        for s1 = (make-string i :initial-element #\x)
#        for expected = (concatenate 'string s1 "," s1)
#        for s2 = (format nil "~,,1,',<~A~;~A~>" s1 s1)
#        unless (string= s2 expected)
#        collect (list i expected s2))
#  nil)

def_pprint_test 'format.justify.5' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  my $collector = _make_collector( $remainder );
  for my $i ( 1 .. 20 ) {
    my $s1 = make_string( $i, initial_element => "x" );
    my $expected = concatenate( $s1, ',', $s1 );
    my $s2 = $f->format( undef, "~,,1,',<~A~;~A~>", $s1, $s1 );
    unless ( $s2 eq $expected ) {
      $collector->( list( $i, $expected, $s2 ) );
    }
  }
  return $remainder;
}, [];

#(def-pprint-test format.justify.6
#  (loop for i from 1 to 20
#        for s1 = (make-string i :initial-element #\x)
#        for expected = (concatenate 'string s1 "  " s1)
#        for s2 = (format nil "~,,2<~A~;~A~>" s1 s1)
#        unless (string= s2 expected)
#        collect (list i expected s2))
#  nil)

def_pprint_test 'format.justify.6' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  my $collector = _make_collector( $remainder );
  for my $i ( 1 .. 20 ) {
    my $s1 = make_string( $i, initial_element => "x" );
    my $expected = concatenate( $s1, ' ', $s1 );
    my $s2 = $f->format( undef, "~,,2<~A~;~A~>", $s1, $s1 );
    unless ( $s2 eq $expected ) {
      $collector->( list( $i, $expected, $s2 ) );
    }
  }
  return $remainder;
}, [];

#(def-pprint-test format.justify.7
#  (loop for mincol = (random 50)
#        for len = (random 50)
#        for s1 = (make-string len :initial-element #\x)
#        for s2 = (format nil "~v<~A~>" mincol s1)
#        for expected = (if (< len mincol)
#                           (concatenate 'string
#                                        (make-string (- mincol len) :initial-element #\Space)
#                                        s1)
#                         s1)
#        repeat 100
#        unless (string= s2 expected)
#        collect (list mincol len s1 s2 expected))
#  nil)

#(def-pprint-test format.justify.8
#  (loop for mincol = (random 50)
#        for minpad = (random 10)
#        for len = (random 50)
#        for s1 = (make-string len :initial-element #\x)
#        for s2 = (format nil "~v,,v<~A~>" mincol minpad s1)
#        for expected = (if (< len mincol)
#                           (concatenate 'string
#                                        (make-string (- mincol len) :initial-element #\Space)
#                                        s1)
#                         s1)
#        repeat 100
#        unless (string= s2 expected)
#        collect (list mincol minpad len s1 s2 expected))
#  nil)

#(def-pprint-test format.justify.9
#  (loop for mincol = (random 50)
#        for padchar = (random-from-seq +standard-chars+)
#        for len = (random 50)
#        for s1 = (make-string len :initial-element #\x)
#        for s2 = (format nil "~v,,,v<~A~>" mincol padchar s1)
#        for expected = (if (< len mincol)
#                           (concatenate 'string
#                                        (make-string (- mincol len) :initial-element padchar)
#                                        s1)
#                         s1)
#        repeat 100
#        unless (string= s2 expected)
#        collect (list mincol padchar len s1 s2 expected))
#  nil)

#(def-pprint-test format.justify.10
#  (loop for mincol = (random 50)
#        for padchar = (random-from-seq +standard-chars+)
#        for len = (random 50)
#        for s1 = (make-string len :initial-element #\x)
#        for s2 = (format nil (format nil "~~~d,,,'~c<~~A~~>" mincol padchar) s1)
#        for expected = (if (< len mincol)
#                           (concatenate 'string
#                                        (make-string (- mincol len) :initial-element padchar)
#                                        s1)
#                         s1)
#        repeat 500
#        unless (string= s2 expected)
#        collect (list mincol padchar len s1 s2 expected))
#  nil)

#(def-pprint-test format.justify.11
#  (loop for i = (1+ (random 20))
#        for colinc = (1+ (random 10))
#        for s1 = (make-string i :initial-element #\x)
#        for s2 = (format nil "~,v<~A~>" colinc s1)
#        for expected-len = (* colinc (ceiling i colinc))
#        for expected = (concatenate 'string
#                                    (make-string (- expected-len i) :initial-element #\Space)
#                                    s1)
#        repeat 10
#        unless (string= expected s2)
#        collect (list i colinc expected s2))
#  nil)

#(def-pprint-test format.justify.12
#  (format nil "~<XXXXXX~^~>")
#  "")

def_pprint_test 'format.justify.12' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~<XXXXXX~^~>"
  );
}, '';

#(def-pprint-test format.justify.13
#  (format nil "~<XXXXXX~;YYYYYYY~^~>")
#  "XXXXXX")

def_pprint_test 'format.justify.13' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~<XXXXXX~;YYYYYYY~^~>"
  );
}, "XXXXXX";

#(def-pprint-test format.justify.13a
#  (format nil "~<~<XXXXXX~;YYYYYYY~^~>~>")
#  "XXXXXX")

def_pprint_test 'format.justify.13a' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~<~<XXXXXX~;YYYYYYY~^~>~>"
  );
}, "XXXXXX";

#(def-pprint-test format.justify.14
#  (format nil "~<XXXXXX~;YYYYYYY~^~;ZZZZZ~>")
#  "XXXXXX")

def_pprint_test 'format.justify.14' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~<XXXXXX~;YYYYYYY~^~;ZZZZZ~>"
  );
}, "XXXXXX";

#(def-pprint-test format.justify.15
#  (format nil "~13,,2<aaa~;bbb~;ccc~>")
#  "aaa  bbb  ccc")

def_pprint_test 'format.justify.15' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~13,,2<aaa~;bbb~;ccc~>"
  );
}, "aaa  bbb  ccc";

#(def-pprint-test format.justify.16
#  (format nil "~10@<abcdef~>")
#  "abcdef    ")

def_pprint_test 'format.justify.16' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~10@<abcdef~>"
  );
}, "abcdef    ";

#(def-pprint-test format.justify.17
#  (format nil "~10:@<abcdef~>")
#  "  abcdef  ")

def_pprint_test 'format.justify.17' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~10:@<abcdef~>"
  );
}, "  abcdef  ";

#(def-pprint-test format.justify.18
#  (format nil "~10:<abcdef~>")
#  "    abcdef")

def_pprint_test 'format.justify.18' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~10:<abcdef~>"
  );
}, "    abcdef";

#(def-pprint-test format.justify.19
#  (format nil "~4@<~>")
#  "    ")

def_pprint_test 'format.justify.19' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~4@<~>"
  );
}, "    ";

#(def-pprint-test format.justify.20
#  (format nil "~5:@<~>")
#  "     ")

def_pprint_test 'format.justify.20' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~5:@<~>"
  );
}, "     ";

#(def-pprint-test format.justify.21
#  (format nil "~6:<~>")
#  "      ")

def_pprint_test 'format.justify.21' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~6:<~>"
  );
}, "      ";

#(def-pprint-test format.justify.22
#  (format nil "~v<~A~>" nil "XYZ")
#  "XYZ")


#(def-pprint-test format.justify.23
#  (format nil "~,v<~A~;~A~>" nil "ABC" "DEF")
#  "ABCDEF")

#(def-pprint-test format.justify.24
#  (format nil "~,,v<~A~;~A~>" nil "ABC" "DEF")
#  "ABCDEF")

#(def-pprint-test format.justify.25
#  (format nil "~,,1,v<~A~;~A~>" nil "ABC" "DEF")
#  "ABC DEF")

#(def-pprint-test format.justify.26
#  (format nil "~,,1,v<~A~;~A~>" #\, "ABC" "DEF")
#  "ABC,DEF")

#(def-pprint-test format.justify.27
#  (format nil "~6<abc~;def~^~>")
#  "   abc")

def_pprint_test 'format.justify.27' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~6<abc~;def~^~>"
  );
}, "   abc";

#(def-pprint-test format.justify.28
#  (format nil "~6@<abc~;def~^~>")
#  "abc   ")

def_pprint_test 'format.justify.28' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~6@<abc~;def~^~>"
  );
}, "abc   ";

### ~:; tests

#(def-pprint-test format.justify.29
#  (format nil "~%X ~,,1<~%X ~:;AAA~;BBB~;CCC~>")
#  "
#X AAA BBB CCC")

def_pprint_test 'format.justify.29' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~%X ~,,1<~%X ~:;AAA~;BBB~;CCC~>"
  );
}, "\nX AAA BBB CCC";

#(def-pprint-test format.justify.30
#  (format nil "~%X ~<~%X ~0,3:;AAA~>~<~%X ~0,3:;BBB~>~<~%X ~0,3:;CCC~>")
#  "
#X 
#X AAA
#X BBB
#X CCC")

def_pprint_test 'format.justify.30' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~%X ~<~%X ~0,3:;AAA~>~<~%X ~0,3:;BBB~>~<~%X ~0,3:;CCC~>"
  );
}, "\nX\nX AAA\nX BBB\nX CCC";

#(def-pprint-test format.justify.31
#  (format nil "~%X ~<~%X ~0,30:;AAA~>~<~%X ~0,30:;BBB~>~<~%X ~0,30:;CCC~>")
#  "
#X AAABBBCCC")

def_pprint_test 'format.justify.31' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~%X ~<~%X ~0,30:;AAA~>~<~%X ~0,30:;BBB~>~<~%X ~0,30:;CCC~>"
  );
}, "\nX AAABBBCCC";

#(def-pprint-test format.justify.32
#  (format nil "~%X ~<~%X ~0,3:;AAA~>,~<~%X ~0,3:;BBB~>,~<~%X ~0,3:;CCC~>")
#  "
#X 
#X AAA,
#X BBB,
#X CCC")

def_pprint_test 'format.justify.32' => sub {
  my $f = JGoff::Lisp::Format->new;
  return $f->format(
    undef,
    "~%X ~<~%X ~0,3:;AAA~>,~<~%X ~0,3:;BBB~>,~<~%X ~0,3:;CCC~>"
  );
}, "\nX \nX AAA,\nX BBB,\nX CCC";

### Error cases

### See 22.3.5.2

### Interaction with ~W

#(deftest format.justify.error.w.1
#  (signals-error-always (format nil "~< ~W ~>" nil) error)
#  t t)

#(deftest format.justify.error.w.2
#  (signals-error-always (format nil "~<X~:;Y~>~W" nil) error)
#  t t)

#(deftest format.justify.error.w.3
#  (signals-error-always (format nil "~w~<X~:;Y~>" nil) error)
#  t t)

### Interaction with ~_

#(deftest format.justify.error._.1
#  (signals-error-always (format nil "~< ~_ ~>") error)
#  t t)

#(deftest format.justify.error._.2
#  (signals-error-always (format nil "~<X~:;Y~>~_") error)
#  t t)

#(deftest format.justify.error._.3
#  (signals-error-always (format nil "~_~<X~:;Y~>") error)
#  t t)

### Interaction with ~I

#(deftest format.justify.error.i.1
#  (signals-error-always (format nil "~< ~i ~>") error)
#  t t)

#(deftest format.justify.error.i.2
#  (signals-error-always (format nil "~<X~:;Y~>~I") error)
#  t t)

#(deftest format.justify.error.i.3
#  (signals-error-always (format nil "~i~<X~:;Y~>") error)
#  t t)

}
