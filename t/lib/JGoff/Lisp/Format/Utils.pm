package JGoff::Lisp::Format::Utils;

use JGoff::Lisp::Format;
use JGoff::Lisp::Format::Utils::Character; # XXX
use Test::More;
use Carp qw( croak );
use YAML;

use base 'Exporter';
our @EXPORT = qw(
  string
  subseq
  char_name
  def_format_test
  deftest
  formatter_call_to_string
  collect
  concatenate
  with_standard_io_syntax
);

our $most_positive_fixnum = ~0; # XXX Probably wrong
our $most_negative_fixnum = -(~0); # XXX Probably wrong
our @standard_chars =
  map { JGoff::Lisp::Format::Utils::Character->new( character => $_ ) }
      split //, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789~!@#$%^&*()_+|\\=-`{}[]:\";'<>?,./\n";
our $char_code_limit = 1114112; # XXX Don't ask me, from sbcl.

sub char_name {
  my ( $char ) = @_;
  return 'Space' if $char and $char eq ' ';
  return $char;
}

sub string {
  my ( $c ) = shift;
  return $c unless ref( $c ) =~ /Character/;
  return $c->toString;
}

sub subseq {
  my ( $str, $start, $end ) = @_;
  substr( $str, $start, $end - $start );
}

sub with_standard_io_syntax(&) {
  my $fn = shift;
  return $fn->();
}

sub concatenate {
  return join '', @_;
}

sub collect {
  my $list = shift;
  push @$list, [ @_ ];
}

# XXX This needs lots of work, there's lots going on in the original function.
sub formatter_call_to_string {
  my ( $fn, $args ) = @_;
  return $fn->( undef, $args );
}

sub def_format_test {
  my ( $name, $format, $args, $result, $num_left ) = @_; # $num_left optional
  my ( $package, $filename, $line ) = caller();
  my $f = JGoff::Lisp::Format->new;
  my $stream = undef;
  if ( ref( $result ) and ref( $result ) eq 'CODE' ) {
    is_deeply( $f->format( $stream, $format, $args ), $result->(), $name ) or
      diag( "  at test file $filename line $line ($format)" );
  }
  elsif ( ref( $result ) ) {
    is_deeply( $f->format( $stream, $format, $args ), $result, $name ) or
      diag( "  at test file $filename line $line ($format)" );
  }
  else {
    is( $f->format( $stream, $format, $args ), $result, $name ) or
      diag( "  at test file $filename line $line ($format)" );
  }
#  if ( $num_left ) {
#    is( scalar @$args, $num_left, $name . " argument count" ) or
#      diag( "  at test file $filename line $line ($format)" );
#  }
}

sub deftest {
  my ( $name, $func, $result ) = @_;
  my ( $package, $filename, $line ) = caller();
  my $test = $func->();
  if ( ref $test ) {
    is_deeply( $test, $result, $name ) or
      diag( "  at test file $filename line $line\n" . Dump( $test ) );
  }
  else {
    is( $test, $result, $name ) or
      diag( "  at test file $filename line $line" );
  }
}

1;
