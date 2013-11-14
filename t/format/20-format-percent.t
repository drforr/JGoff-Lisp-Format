#!perl

use Test::More tests => 10;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

# (def-format-test format.%.1
#   "~%" nil #.(string #\Newline))

def_format_test 'format.%.1' =>
  "~%",
  undef,
  "\n";

SKIP: {
  my $count = 1;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;

#(deftest format.%.2
#  (loop for i from 0 to 100
#        for s1 = (make-string i :initial-element #\Newline)
#        for format-string = (format nil "~~~D%" i)
#        for s2 = (format nil format-string)
#        for fn = (eval `(formatter ,s2))
#        for s3 = (formatter-call-to-string fn)
#        unless (and (string= s1 s2) (string= s1 s3))
#        collect i)
#  nil)

deftest 'format.%.2' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 1 .. 100 ) {
    my $s1 = make_string( $i, initial_element => "\n" );
    my $format_string = $f->format( undef, "~~~D%", $i );
    my $s2 = $f->format( undef, $format_string );
    my $fn = $f->formatter( $s2 );
    my $s3 = formatter_call_to_string( $fn );
    unless ( $s1 eq $s2 and $s1 eq $s3 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];
}

# (def-format-test format.%.3
#   "~v%" (nil) #.(string #\Newline))

def_format_test 'format.%.3' =>
  "~v%",
  [ undef ],
  "\n";

# (def-format-test format.%.4
#   "~V%" (1) #.(string #\Newline))

def_format_test 'format.%.4' =>
  "~V%",
  [ 1 ],
  "\n";

#(deftest format.%.5
#  (loop for i from 0 to 100
#        for s1 = (make-string i :initial-element #\Newline)
#        for s2 = (format nil "~v%" i)
#        unless (string= s1 s2)
#        collect i)
#  nil)

deftest 'format.%.5' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 0 .. 100 ) {
    my $s1 = make_string( $i, initial_element => "\n" );
    my $s2 = $f->format( undef, "~v%", $i );
    unless( $s1 eq $s2 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];

#(deftest formatter.%.5
#  (let ((fn (formatter "~v%")))
#    (loop for i from 0 to 100
#          for s1 = (make-string i :initial-element #\Newline)
#          for s2 = (formatter-call-to-string fn i)
#          unless (string= s1 s2)
#          collect i))
#  nil)

deftest 'formatter.%.5' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~v%" );
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 0 .. 100 ) {
    my $s1 = make_string( $i, initial_element => "\n" );
    my $s2 = formatter_call_to_string( $fn, $i );
    unless( $s1 eq $s2 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];

SKIP: {
  my $count = 2;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;

#(deftest format.%.6
#  (loop for i from 0 to (min (- call-arguments-limit 3) 100)
#        for args = (make-list i)
#        for s1 = (make-string i :initial-element #\Newline)
#        for s2 = (apply #'format nil "~#%" args)
#        unless (string= s1 s2)
#        collect i)
#  nil)

deftest 'format.%.5' => sub {
  my $f = JGoff::Lisp::Format->new;
  my ( $remainder, $collector ) = _make_collector;
  for my $i ( 1 .. min( $JGoff::Lisp::Format::Utils::call_arguments_limit - 3,
                        100 ) ) {
    my $args = make_list( $i );
    my $s1 = make_string( $i, initial_element => "\n" );
    my $s2 = apply( sub { $f->format( @_ ) }, undef, "~#%", $args );
    unless ( $s1 eq $s2 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];

#(deftest formatter.%.6
#  (let ((fn (formatter "~#%")))
#    (loop for i from 0 to (min (- call-arguments-limit 3) 100)
#          for args = (make-list i)
#          for s1 = (make-string i :initial-element #\Newline)
#          for s2 = (with-output-to-string
#                     (stream)
#                     (assert (equal (apply fn stream args) args)))
#          unless (string= s1 s2)
#          collect i))
#  nil)

deftest 'formatter.%.5' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~#%" );
  my ( $remainder, $collector ) = _make_collector;
  my $stream = []; # XXX Think about this
  for my $i ( 1 .. min( $JGoff::Lisp::Format::Utils::call_arguments_limit - 3,
                        100 ) ) {
    my $args = make_list( $i );
    my $s1 = make_string( $i, initial_element => "\n" );
    my $s2 = with_output_to_string $stream,  sub {
      assert( apply( $fn, $stream, $args ) eq $args ); # XXX XXX
    };
    unless ( $s1 eq $s2 ) {
      $collector->( $i );
    }
  }
  return $remainder;
}, [];
}
