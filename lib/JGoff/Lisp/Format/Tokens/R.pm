package JGoff::Lisp::Format::Tokens::R;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has arguments => ( is => 'rw' );
has colon => ( is => 'ro' );
has at => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core ) = @_;
  $self->_resolve_arguments(
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

  my $argument;
  if ( $self->{'radix-v'} ) {
    $argument = $core->current_argument;
    $core->forward_argument;
    $argument = $english_number_names[$argument];
  }
  else {
    $argument = $core->current_argument;
    $core->forward_argument;
    $argument = $self->_argument_to_base( $self->{radix}, $argument );
  }

  return $argument;
}

1;
