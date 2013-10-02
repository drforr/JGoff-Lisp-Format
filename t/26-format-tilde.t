#!perl

use Test::More tests => 10;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

# (def-format-test format.~.1
#   "~~" nil "~")

def_format_test 'format.~.1' =>
  "~~",
  undef,
  "~";

SKIP: {
  my $count = 2;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

#(deftest format.~.2
#  (loop for i from 0 to 100
#        for s = (make-string i :initial-element #\~)
#        for format-string = (format nil "~~~D~~" i)
#        for s2 = (format nil format-string)
#        unless (string= s s2)
#        collect (list i s s2))
#  nil)

deftest 'format.~.2' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 0 .. 100 ) {
    my $s = make_string( $i, initial_element => "~" );
    my $format_string = $f->format( undef, "~~~D~~", $i );
    my $s2 = $f->format( undef, $format_string );
    unless ( $s eq $s2 ) {
      $collector->( list( $i, $s, $s2 ) );
    }
  }
  return $remainder;
}, [];

#(deftest formatter.~.2
#  (loop for i from 0 to 100
#        for s = (make-string i :initial-element #\~)
#        for format-string = (format nil "~~~D~~" i)
#        for fn = (eval `(formatter ,format-string))
#        for s2 = (formatter-call-to-string fn)
#        unless (string= s s2)
#        collect (list i s s2))
#  nil)

deftest 'formatter.~.2' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 0 .. 100 ) {
    my $s = make_string( $i, initial_element => "~" );
    my $format_string = $f->format( undef, "~~~D~~", $i );
    my $fn = $f->formatter( $format_string );
    my $s2 = formatter_call_to_string( $fn );
    unless ( $s eq $s2 ) {
      $collector->( list( $i, $s, $s2 ) );
    }
  }
  return $remainder;
}, [];

}

# (def-format-test format.~.3
#   "~v~" (0) "")

def_format_test 'format.~.3' =>
  "~v~",
  [ 0 ],
  "";

SKIP: {
  my $count = 4;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

#(deftest format.~.4
#  (loop for i from 0 to 100
#        for s = (make-string i :initial-element #\~)
#        for s2 = (format nil "~V~" i)
#        unless (string= s s2)
#        collect (list i s s2))
#  nil)

deftest 'format.~.2' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 0 .. 100 ) {
    my $s = make_string( $i, initial_element => "~" );
    my $s2 = $f->format( undef, "~V~", $i );
    unless ( $s eq $s2 ) {
      $collector->( list( $i, $s, $s2 ) );
    }
  }
  return $remainder;
}, [];

#(deftest formatter.~.4
#  (let ((fn (formatter "~v~")))
#    (loop for i from 0 to 100
#          for s = (make-string i :initial-element #\~)
#          for s2 = (formatter-call-to-string fn i)
#          unless (string= s s2)
#          collect (list i s s2)))
#  nil)

deftest 'formatter.~.2' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn= $f->formatter( "~v~" );
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 0 .. 100 ) {
    my $s = make_string( $i, initial_element => "~" );
    my $s2 = formatter_call_to_string( $fn, $i );
    unless ( $s eq $s2 ) {
      $collector->( list( $i, $s, $s2 ) );
    }
  }
  return $remainder;
}, [];

#(deftest format.~.5
#  (loop for i from 0 to (min (- call-arguments-limit 3) 100)
#        for s = (make-string i :initial-element #\~)
#        for args = (make-list i)
#        for s2 = (apply #'format nil "~#~" args)
#        unless (string= s s2)
#        collect (list i s s2))
#  nil)

deftest 'format.~.5' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 1 .. min( $JGoff::Lisp::Format::Utils::call_arguments_limit - 3,
                        100 ) ) {
    my $s = make_string( $i, initial_element => "~" );
    my $args = make_list( $i );
    my $s2 = apply( sub { $f->format( @_ ) }, undef, "~#~", @$args );
    unless ( $s eq $s2 ) {
      $collector->( list( $i, $s, $s2 ) );
    }
  }
  return $remainder;
}, [];

#(deftest formatter.~.5
#  (let ((fn (formatter "~#~")))
#    (loop for i from 0 to (min (- call-arguments-limit 3) 100)
#          for s = (make-string i :initial-element #\~)
#          for args = (make-list i)
#          for s2 = (with-output-to-string
#                     (stream)
#                     (assert (equal (apply fn stream args) args)))
#          unless (string= s s2)
#          collect (list i s s2)))
#  nil)
}
