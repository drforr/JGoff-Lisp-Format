#!perl

use Test::More tests => 13;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

### Test of the ~C directive

#(deftest format.c.1
#  (loop for c across +standard-chars+
#        for s = (format nil "~C" c)
#        unless (string= s (string c))
#        collect (list c s))
#  nil)

deftest 'format.c.1' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $c ( @JGoff::Lisp::Format::Utils::standard_chars ) {
    my $s = $f->format( undef, "~C", $c->toString ); # JMG XXX hack
    unless ( string( $c ) eq $s ) {
      collect( $remainder, list( $c, $s ) );
    }
  }
  return $remainder;
}, [];

SKIP: {
  my $count = 3;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;

#(deftest format.c.1a
#  (loop with count = 0
#        for i from 0 below (min #x10000 char-code-limit)
#        for c = (code-char i)
#        for s = (and c (format nil "~c" c))
#        unless (or (not c)
#                   (not (eql (char-code c) (char-int c)))
#                   (string= s (string c)))
#        do (incf count) and collect (list i c s)
#        when (> count 100) collect "count limit exceeded" and do (loop-finish))
#  nil)

deftest 'format.c.1a' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $i ( 0 .. min( 0x10_000,
                        $JGoff::Lisp::Format::Utils::char_code_limit ) - 1 ) {
    my $c = code_char( $i );
    my $s = $c and $f->format( undef, "~c", $c );
    unless ( !defined $c or
            !( char_code( $c ) eq char_int( $c ) ) or
            ( $s eq string( $c ) ) ) {
      $count++;
      collect( $remainder, list( $i, $c, $s ) );
    }
    if ( $count > 100 ) {
      collect( "count limit exceeded" );
      last;
    }
  }
  return $remainder;
}, [];

#(deftest format.c.2
#  (loop for c across +standard-chars+
#        for s = (format nil "~:c" c)
#        unless (or (not (graphic-char-p c))
#                   (eql c #\Space)
#                   (string= s (string c)))
#        collect (list c s))
#  nil)

deftest 'format.c.2' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $c ( @JGoff::Lisp::Format::Utils::standard_chars ) {
    my $s = $f->format( undef, "~:c", $c );
    unless ( !graphic_char_p( $c ) or
             ( $c eq ' ' ) or
             ( $s eq $c ) ) {
      collect( $remainder, list( $c, $s ) );
    }
  }
  return $remainder;
}, [];

#(deftest format.c.2a
#  (loop with count = 0
#        for i from 0 below (min #x10000 char-code-limit)
#        for c = (code-char i)
#        for s = (and c (format nil "~:C" c))
#        unless (or (not c)
#                   (not (eql (char-code c) (char-int c)))
#                   (not (graphic-char-p c))
#                   (eql c #\Space)
#                   (string= s (string c)))
#        do (incf count) and collect (list i c s)
#        when (> count 100) collect "count limit exceeded" and do (loop-finish))
#  nil)

deftest 'format.c.2a' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $count = 0;
  my $remainder = [];
  for my $i ( min( 0x10_000,
                   $JGoff::Lisp::Format::Utils::char_code_limit ) - 1 ) {
    my $c = code_char( $i );
    my $s = $c and $f->format( undef, "~:C", $c );
    unless ( !defined $c or
             !( char_code( $c ) eq char_int( $c ) ) or
             !graphic_char_p( $c ) or
             ( $c eq ' ' ) or
             ( $s eq string( $c ) ) ) {
      $count++;
      collect( $remainder, list( $i, $c, $s ) );
    }
    if ( $count > 100 ) {
      collect( $remainder, "count limit exceeded" );
      last;
    }
  }
  return $remainder;
}, [];

}

# (def-format-test format.c.3
#   "~:C" (#\Space) #.(char-name #\Space))

def_format_test 'format.c.3' =>
  "~:C",
  [ " " ],
  char_name( ' ' );

SKIP: {
  my $count = 6;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

#(deftest format.c.4
#  (loop for c across +standard-chars+
#        for s = (format nil "~:C" c)
#        unless (or (graphic-char-p c)
#                   (string= s (char-name c)))
#        collect (list c (char-name c) s))
#  nil)

deftest 'format.c.4' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $count = 0;
  my $remainder = [];
  for my $c ( @JGoff::Lisp::Format::Utils::standard_chars ) {
    my $s = $f->format( undef, "~:C", $c );
    unless ( graphic_char_p( $c ) or
             ( $s eq char_name( $c ) ) ) {
      collect( $remainder, list( $c, char_name( $c ), $s ) );
    }
  }
  return $remainder;
}, [];

#(deftest format.c.4a
#  (loop with count = 0
#        for i from 0 below (min #x10000 char-code-limit)
#        for c = (code-char i)
#        for s = (and c (format nil "~:c" c))
#        unless (or (not c)
#                   (not (eql (char-code c) (char-int c)))
#                   (graphic-char-p c)
#                   (string= s (char-name c)))
#        do (incf count) and collect (print (list i c s))
#        when (> count 100) collect "count limit exceeded" and do (loop-finish))
#  nil)

deftest 'format.c.4a' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $count = 0;
  my $remainder = [];
  for my $i ( min( 0x10_000,
                   $JGoff::Lisp::Format::Utils::char_code_limit ) - 1 ) {
    my $c = code_char( $i );
    my $s = $c and $f->format( undef, "~:c", $c );
    unless ( !defined $c or
             !( char_code( $c ) eq char_int( $c ) ) or
             !graphic_char_p( $c ) or
             ( $c eq ' ' ) or
             ( $s eq char_name( $c ) ) ) {
      $count++;
      collect( $remainder, list( $i, $c, $s ) );
    }
    if ( $count > 100 ) {
      collect( $remainder, "count limit exceeded" );
      last;
    }
  }
  return $remainder;
}, [];

#(deftest format.c.5
#  (loop for c across +standard-chars+
#        for s = (format nil "~@c" c)
#        for c2 = (read-from-string s)
#        unless (eql c c2)
#        collect (list c s c2))
#  nil)

#(deftest format.c.5a
#  (loop with count = 0
#        for i from 0 below (min #x10000 char-code-limit)
#        for c = (code-char i)
#        for s = (and c (format nil "~@C" c))
#        for c2 = (and c (read-from-string s))
#        unless (eql c c2)
#        do (incf count) and collect (list c s c2)
#        when (> count 100) collect "count limit exceeded" and do (loop-finish))
#  nil)

deftest 'format.c.5a' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $i ( 0 .. min( 0x10_000,
                        $JGoff::Lisp::Format::Utils::char_code_limit ) - 1 ) {
    my $c = code_char( $i );
    my $s = $c and $f->format( undef, '~@C', $c );
    my $c2 = $c and read_from_string( $s );
    unless ( $c eq $c2 ) { # XXX
      die "comparing character objects...";
      $count++;
      collect( $remainder, list( $c, $s, $c2 ) );
    }
    if ( $count > 100 ) {
      collect( $remainder, "count limit exceeded" );
      last;
    }
  }
  return $remainder;
}, [];

#(deftest format.c.6
#  (loop for c across +standard-chars+
#        for s1 = (format nil "~:C" c)
#        for s2 = (format nil "~:@C" c)
#        unless (eql (search s1 s2) 0)
#        collect (list c s1 s2))
#  nil)

deftest 'format.c.6' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $c ( @JGoff::Lisp::Format::Utils::standard_chars ) {
    my $s1 = $f->format( undef, "~:C", $c );
    my $s2 = $f->format( undef, '~:@C', $c );
    unless ( search( $s1, $s2 ) == 0 ) {
      collect( $remainder, list( $c, $s1, $s2 ) );
    }
  }
  return $remainder;
}, [];

#(deftest format.c.6a
#  (loop with count = 0
#        for i from 0 below (min #x10000 char-code-limit)
#        for c = (code-char i)
#        for s1 = (and c (format nil "~:C" c))
#        for s2 = (and c (format nil "~@:C" c))
#        unless (or (not c) (eql (search s1 s2) 0))
#        do (incf count) and collect (list c s1 s2)
#        when (> count 100) collect "count limit exceeded" and do (loop-finish))
#  nil)

deftest 'format.c.6a' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $count = 0;
  my $remainder = [];
  for my $i ( 0 .. min( 0x10_000,
                        $JGoff::Lisp::Format::Utils::char_code_limit ) - 1 ) {
    my $c = code_char( $i );
    my $s1 = $c and $f->format( undef, "~:C", $c );
    my $s2 = $c and $f->format( undef, '~@:C', $c );
    unless ( !defined $c or
             ( search( $s1, $2 ) == 0 ) ) {
      $count++;
      collect( $remainder, list( $c, $s1, $s2 ) );
    }
    if ( $count > 100 ) {
      collect( $remainder, "count limit exceeded" );
      last;
    }
  }
  return $remainder;
}, [];
}
