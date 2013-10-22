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
  char_code
  char_int
  def_format_test
  def_pprint_test
  deftest
  formatter_call_to_string
  collect
  _make_collector
  list
  concatenate
  with_standard_io_syntax
  code_char
  make_string
  random_from_seq
  remove_duplicates
  graphic_char_p
  read_from_string
  search
  make_list
  apply
  cons
  signals_type_error
  signals_error
  with_output_to_string
  assert
  ash
  random
);

our $most_positive_fixnum = ~0; # XXX Probably wrong
our $most_negative_fixnum = -(~0); # XXX Probably wrong
our @standard_chars =
  map { JGoff::Lisp::Format::Utils::Character->new( character => $_ ) }
      split //, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789~!@#$%^&*()_+|\\=-`{}[]:\";'<>?,./\n";
our $char_code_limit = 1114112; # XXX Don't ask me, from sbcl.
# XXX The real argument limit from SBCL is 4611686018427387903, so fake it.
our $call_arguments_limit = 1114112;
our @mini_universe = ( ); # XXX UNDEFINED DAMNIT

sub char_name {
  my ( $char ) = @_;
  return 'Space' if $char and $char eq ' ';
  return 'Newline' if $char and $char eq "\n";
  return $char;
}

sub char_code {
  my ( $char ) = @_;
  return ord( $char );
}

sub char_int {
  my ( $char ) = @_;
die "char_int not implemented yet - Not sure what the difference is vis-a-vis char_code";
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
  push @$list, @_;
}

sub _make_collector {
  my $remainder = [];
  return ( $remainder, sub {
    push @$remainder, @_;
  } );
}

sub list {
  my @list = @_;
  return [ @list ];
}

# XXX This needs lots of work, there's lots going on in the original function.
sub formatter_call_to_string {
  my ( $fn, @args ) = @_;
  return $fn->( undef, @args );
}

sub def_format_test {
  my ( $name, $format, $args, $result, $num_left ) = @_; # $num_left optional
  my ( $package, $filename, $line ) = caller();
  my $f = JGoff::Lisp::Format->new;
  my $stream = undef;
  if ( ref( $result ) and ref( $result ) eq 'CODE' ) {
    is_deeply( $f->format( $stream, $format, @$args ), $result->(), $name ) or
      diag( "  at test file $filename line $line ($format)" );
  }
  elsif ( ref( $result ) ) {
    is_deeply( $f->format( $stream, $format, @$args ), $result, $name ) or
      diag( "  at test file $filename line $line ($format)" );
  }
  else {
    is( $f->format( $stream, $format, @$args ), $result, $name ) or
      diag( "  at test file $filename line $line ($format)" );
  }
#  if ( $num_left ) {
#    is( scalar @$args, $num_left, $name . " argument count" ) or
#      diag( "  at test file $filename line $line ($format)" );
#  }
}

sub def_pprint_test {
  die "NOT IMPLEMENTED YET!";
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

sub code_char {
  my $code = shift;
  return chr( $code );
}

sub make_string {
  my $count = shift;
  my %args = @_;
  if ( $args{initial_element} ) {
    return $args{initial_element} x $count;
  }
}

sub random_from_seq {
  my @seq = @_;
  return $seq[rand @seq];
}

sub remove_duplicates {
  my @seq = @_;
  my %dedup;
  return grep { !$dedup{$_}++ } @seq;
}

sub graphic_char_p {
  die "graphic_char_p not implemented yet!";
}

sub read_from_string {
  die "read_from_string not implemented yet!";
}

sub search {
  my ( $in, $str ) = @_;

  my $idx = index( $str, $in );
  return $idx >= 0 ? $idx : undef;
}

sub make_list {
  my $count = shift;
  return [ (undef) x $count ];
}

sub apply {
  my $fn = shift;
  my @args = @_;
  return $fn->( @args );
}

sub cons {
  my $element = shift;
  my $list = shift;
  return [ @$list, $element ];
}

sub signals_type_error {
  die "signals_type_error not implemented yet!";
}

sub signals_error {
  die "signals_error not implemented yet!";
}

sub with_output_to_string {
  die "with_output_to_string not implemented yet!";
}

sub assert {
  die "assert not implemented yet!";
}

sub ash {
  my ( $shift, $value ) = @_;
  $value *= 2**$shift;
}

sub random {
  my $value = shift;
  return rand( $value );
}

1;
