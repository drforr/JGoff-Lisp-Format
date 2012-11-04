package JGoff::Lisp::Format::Utils;

use JGoff::Lisp::Format;
use Test::More;

use base 'Exporter';
our @EXPORT = qw(
  def_format_test
  deftest
  formatter_call_to_string collect
  with_standard_io_syntax
);

our $most_positive_fixnum = ~0; # XXX Probably wrong
our $most_negative_fixnum = -(~0); # XXX Probably wrong
our @standard_chars = split //, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789~!@#$%^&*()_+|\\=-`{}[]:\";'<>?,./\n";
our $char_code_limit = 1114112; # XXX Don't ask me, from sbcl.

sub with_standard_io_syntax(&) {
  my $fn = shift;
  return $fn->();
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
  my $f = JGoff::Lisp::Format->new;
  my $stream = undef;
  if ( ref( $result ) and ref( $result ) eq 'CODE' ) {
    is_deeply( $f->format( $stream, $format, $args ), $result->(), $name );
  }
  elsif ( ref( $result ) ) {
    is_deeply( $f->format( $stream, $format, $args ), $result, $name );
  }
  else {
    is( $f->format( $stream, $format, $args ), $result, $name );
  }
  if ( $num_left ) {
    is( scalar @$args, $num_left, $name );
  }
}

sub deftest {
  my ( $name, $func, $result ) = @_;
  my $test = $func->();
  if ( ref $test ) {
    is_deeply( $test, $result, $name );
  }
  else {
    is( $test, $result, $name );
  }
}

1;
