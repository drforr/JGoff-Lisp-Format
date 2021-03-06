#!perl

use Test::More tests => 19;
use List::Util qw( min );

BEGIN {
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

# (def-format-test format.&.1
#   "~0&" nil "")

def_format_test 'format.&.1' =>
  "~0&",
  undef,
  "";

# (def-format-test format.&.2
#   "~&" nil "")

def_format_test 'format.&.2' =>
  "~&",
  undef,
  "";

# (def-format-test format.&.3
#   "X~&" nil #.(concatenate 'string "X" (string #\Newline)))

def_format_test 'format.&.3' =>
  "X~&",
  undef,
  concatenate( "X", "\n" );

# (def-format-test format.&.4
#   "X~%~&" nil #.(concatenate 'string "X" (string #\Newline)))

def_format_test 'format.&.4' =>
  "X~%~&",
  undef,
  concatenate( "X", "\n" );

#
# Test for ~n& beforehand.
#
def_format_test 'format.&.author.1' =>
  "~2&",
  undef,
  "\n" x 1; # JMG And it's worth pointing out that it's one off.

# (deftest format.&.5
#   (loop for i from 1 to 100
#         for s1 = (make-string (1- i) :initial-element #\Newline)
#         for format-string = (format nil "~~~D&" i)
#         for s2 = (format nil format-string)
#         unless (string= s1 s2)
#         collect i)
#   nil)

deftest 'format.&.5' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 1 .. 100 ) {
    my $s1 = make_string( $i - 1, initial_element => "\n" );
    my $format_string = $f->format( undef, "~~~D&", $i );
    my $s2 = $f->format( undef, $format_string );
    unless ( $s1 eq $s2 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];

#(deftest formatter.&.5
#  (loop for i from 1 to 100
#        for s1 = (make-string (1- i) :initial-element #\Newline)
#        for format-string = (format nil "~~~D&" i)
#        for fn = (eval `(formatter ,format-string))
#        for s2 = (formatter-call-to-string fn)
#        unless (string= s1 s2)
#        collect i)
#  nil)

deftest 'formatter.&.5' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 1 .. 100 ) {
    my $s1 = make_string( $i - 1, initial_element => "\n" );
    my $format_string = $f->format( undef, "~~~D&", $i );
    my $fn = $f->formatter( $format_string );
    my $s2 = formatter_call_to_string( $fn );
    unless ( $s1 eq $s2 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];

#(deftest format.&.6
#  (loop for i from 1 to 100
#        for s1 = (concatenate 'string
#                              "X"
#                              (make-string i :initial-element #\Newline))
#        for format-string = (format nil "X~~~D&" i)
#        for s2 = (format nil format-string)
#        unless (string= s1 s2)
#        collect i)
#  nil)

deftest 'format.&.6' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 1 .. 100 ) {
    my $s1 = concatenate( "X",
                          make_string( $i, initial_element => "\n" ) );
    my $format_string = $f->format( undef, "X~~~D&", $i );
    my $s2 = $f->format( undef, $format_string );
    unless ( $s1 eq $s2 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];

#(deftest formatter.&.6
#  (loop for i from 1 to 100
#        for s1 = (concatenate 'string
#                              "X"
#                              (make-string i :initial-element #\Newline))
#        for format-string = (format nil "X~~~D&" i)
#        for fn = (eval `(formatter ,format-string))
#        for s2 = (formatter-call-to-string fn)
#        unless (string= s1 s2)
#        collect i)
#  nil)

deftest 'formatter.&.6' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 1 .. 100 ) {
    my $s1 = concatenate( "X", make_string( $i, initial_element => "\n" ) );
    my $format_string = $f->format( undef, "X~~~D&", $i );
    my $fn = $f->formatter( $format_string );
    my $s2 = formatter_call_to_string( $fn );
    unless ( $s1 eq $s2 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];

# (def-format-test format.&.7
#   "~v&" (nil) "")

def_format_test 'format.&.7' =>
  "~v&",
  [ undef ],
  "";

# (def-format-test format.&.8
#   "X~v&" (nil) #.(concatenate 'string "X" (string #\Newline)))

def_format_test 'format.&.8' =>
  "X~v&",
  [ undef ],
  concatenate( "X", "\n" );

#(deftest format.&.9
#  (loop for i from 1 to 100
#        for s1 = (make-string (1- i) :initial-element #\Newline)
#        for s2 = (format nil "~V&" i)
#        unless (string= s1 s2)
#        collect i)
#  nil)

deftest 'format.&.9' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 1 .. 100 ) {
    my $s1 = make_string( $i - 1, initial_element => "\n" );
    my $s2 = $f->format( undef, "~V&", $i );
    unless ( $s1 eq $s2 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];

#(deftest formatter.&.9
#  (let ((fn (formatter "~V&")))
#    (loop for i from 1 to 100
#          for s1 = (make-string (1- i) :initial-element #\Newline)
#          for s2 = (formatter-call-to-string fn i)
#          unless (string= s1 s2)
#          collect i))
#  nil)

deftest 'formatter.&.9' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~V&" );
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 1 .. 100 ) {
    my $s1 = make_string( $i - 1, initial_element => "\n" );
    my $s2 = formatter_call_to_string( $fn, $i );
    unless ( $s1 eq $s2 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];

#(deftest format.&.10
#  (loop for i from 1 to (min (- call-arguments-limit 3) 100)
#        for s1 = (make-string (1- i) :initial-element #\Newline)
#        for args = (make-list i)
#        for s2 = (apply #'format nil "~#&" args)
#        unless (string= s1 s2)
#        collect i)
#  nil)

deftest 'format.&.10' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 1 .. min( $JGoff::Lisp::Format::Utils::call_arguments_limit - 3,
                        100 ) ) {
    my $s1 = make_string( $i - 1, initial_element => "\n" );
    my $args = make_list( $i );
    my $s2 = apply( sub { $f->format( @_ ) }, undef, "~#&", @$args );
    unless ( $s1 eq $s2 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];

SKIP: {
  my $count = 1;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;

#(deftest formatter.&.10
#  (let ((fn (formatter "~#&")))
#    (loop for i from 1 to (min (- call-arguments-limit 3) 100)
#          for s1 = (make-string (1- i) :initial-element #\Newline)
#          for args = (loop for j below i collect j)
#          for s2 = (with-output-to-string
#                     (stream)
#                     (assert (equal (apply fn stream args) args)))
#          unless (string= s1 s2)
#          collect i))
#  nil)

deftest 'formatter.&.10' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~#&" );
  my ( $remainder, $collector ) = _make_collector;
  my $stream = []; # XXX Think about this
  for my $i ( 1 .. min( $JGoff::Lisp::Format::Utils::call_arguments_limit - 3,
                        100 ) ) {
    my $s1 = make_string( $i - 1, initial_element => "\n" );
    my $args = [ 0 .. $i - 1 ]; # XXX More perlish
    my $s2 = with_output_to_string $stream, sub {
      assert( apply( $fn, $stream, $args ) eq $args ); # XXX XXX
    };
    unless ( $s1 eq $s2 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];

}

# (def-format-test format.&.11
#   "X~V%" (0) "X")

def_format_test 'format.&.11' =>
  "X~V%",
  [ 0 ],
  "X";

# (def-format-test format.&.12
#   "X~#%" nil "X")

def_format_test 'format.&.12' =>
  "X~#%",
  undef,
  "X";

# (def-format-test format.&.13
#   "X~#%" ('a 'b 'c) #.(let ((nl (string #\Newline)))
#                         (concatenate 'string "X" nl nl nl))
#   3)

def_format_test 'format.&.13' =>
  "X~#%", [ "a", "b", "c" ], sub {
     my $nl = "\n";
     concatenate( "X", $nl, $nl, $nl ) },
  3;
