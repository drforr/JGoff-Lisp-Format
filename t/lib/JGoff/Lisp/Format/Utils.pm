package JGoff::Lisp::Format::Utils;

use JGoff::Lisp::Format;
use Test::More;

use base 'Exporter';
our @EXPORT = qw( def_format_test deftest );

sub def_format_test {
  my ( $name, $format, $args, $result, $num_left ) = @_; # $num_left optional
  my $f = JGoff::Lisp::Format->new;
  my $stream = undef;
  if ( ref( $result ) and ref( $result ) eq 'CODE' ) {
    is_deeply( $f->format( $stream, $format, @$args ), $result->(), $name );
  }
  elsif ( ref( $result ) ) {
    is_deeply( $f->format( $stream, $format, @$args ), $result, $name );
  }
  else {
    is( $f->format( $stream, $format, @$args ), $result, $name );
  }
}

sub deftest {
  my ( $name, $func, $result ) = @_;
  is( $func->(), $result, $name );
}

1;
