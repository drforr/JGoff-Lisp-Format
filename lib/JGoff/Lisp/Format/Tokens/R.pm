package JGoff::Lisp::Format::Tokens::R;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has parameters => ( is => 'rw' );
has colon => ( is => 'ro', isa => 'Bool' );
has at => ( is => 'ro', isa => 'Bool' );

=head1 NAME

JGoff::Lisp::Format::Tokens::R - Internal token for parser

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 METHODS

=cut

sub _old_roman_numeral {
  my $self = shift;
  my ( $argument ) = @_;
  my ( $n_m, $n_d, $n_c, $n_l, $n_x, $n_v ) = ( 0, 0, 0, 0, 0, 0 );

  while ( $argument >= 1000 ) { $n_m++; $argument -= 1000 }
  while ( $argument >= 500 ) { $n_d++; $argument -= 500 }
  while ( $argument >= 100 ) { $n_c++; $argument -= 100 }
  while ( $argument >= 50 ) { $n_l++; $argument -= 50 }
  while ( $argument >= 10 ) { $n_x++; $argument -= 10 }
  while ( $argument >= 5 ) { $n_v++; $argument -= 5 }
  return ( $n_m ? 'M' x $n_m : '' ).
         ( $n_d ? 'D' x $n_d : '' ).
         ( $n_c ? 'C' x $n_c : '' ).
         ( $n_l ? 'L' x $n_l : '' ).
         ( $n_x ? 'X' x $n_x : '' ).
         ( $n_v ? 'V' x $n_v : '' ).
         ( $argument ? 'I' x $argument : '' );
}

=head2 format( $core )

=cut

sub format {
  my $self = shift;
  my ( $core ) = @_;
  my $has_arguments = $self->parameters and
                      @{ $self->parameters } ? 1 : undef;
  my $not_number = $self->parameters and
                   @{ $self->parameters } and
                   $self->parameters->[0] =~ /[#v]/;
  $self->_resolve_parameters(
    $core, [
      [ 'radix' => 10 ],
      [ 'mincol' => 0 ],
      [ 'padchar' => ' ' ],
#      [ 'colinc' => 1 ],
      [ 'commachar' => ',' ],
      [ 'comma-interval' => 3 ],
    ]
  );
$self->{colinc} = 1;
  my @english_number_names = (
   "zero",
   "one", "two", "three", "four", "five",
   "six", "seven", "eight", "nine", "ten",
   "eleven", "twelve", "thirteen", "fourteen", "fifteen",
   "sixteen", "seventeen", "eighteen", "nineteen", "twenty",
   "twenty-one", "twenty-two", "twenty-three", "twenty-four", "twenty-five",
   "twenty-six", "twenty-seven", "twenty-eight", "twenty-nine", "thirty",
   "thirty-one", "thirty-two", "thirty-three", "thirty-four", "thirty-five",
   "thirty-six", "thirty-seven", "thirty-eight", "thirty-nine", "forty",
   "forty-one", "forty-two", "forty-three", "forty-four", "forty-five",
   "forty-six", "forty-seven", "forty-eight", "forty-nine", "fifty",
   "fifty-one", "fifty-two", "fifty-three", "fifty-four", "fifty-five",
   "fifty-six", "fifty-seven", "fifty-eight", "fifty-nine", "sixty",
   "sixty-one", "sixty-two", "sixty-three", "sixty-four", "sixty-five",
   "sixty-six", "sixty-seven", "sixty-eight", "sixty-nine", "seventy",
   "seventy-one", "seventy-two", "seventy-three", "seventy-four", "seventy-five",
   "seventy-six", "seventy-seven", "seventy-eight", "seventy-nine", "eighty",
   "eighty-one", "eighty-two", "eighty-three", "eighty-four", "eighty-five",
   "eighty-six", "eighty-seven", "eighty-eight", "eighty-nine", "ninety",
   "ninety-one", "ninety-two", "ninety-three", "ninety-four", "ninety-five",
   "ninety-six", "ninety-seven", "ninety-eight", "ninety-nine", "one hundred"
  );

  my $argument = $core->current_argument;
  $core->forward_argument;
  if ( $self->{'radix-v'} and
       !defined( $self->{radix} ) ) {
    $argument = $english_number_names[ $argument ];
  }
  elsif ( $self->at and
          $self->colon and
          not $has_arguments ) {
    $argument = $self->_old_roman_numeral( $argument );
  }
  else {
    $argument = $self->_argument_to_base( $self->{radix}, $argument );
  }

  return $argument;
}

1;
