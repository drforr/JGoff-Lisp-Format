package JGoff::Lisp::Format::Utils;

use Exporter;
our @EXPORT = qw( def_format_test deftest );

sub def_format_test {
  my ( $f, $name, $format, $args, $result, $num_left ) = @_; # $num_left optional
  if ( ref( $result ) and ref( $result ) eq 'CODE' ) {
    is_deeply( $f->format( $format, @$args ), $result->(), $name );
  }
  elsif ( ref( $result ) ) {
    is_deeply( $f->format( $format, @$args ), $result, $name );
  }
  else {
    is( $f->format( $format, @$args ), $result, $name );
  }
}

sub deftest {
  my ( $name, $func, $result ) = @_;
  is( $func->(), $result, $name );
}

1;
