package JGoff::Lisp::Format::Tokens::R;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has parameters => ( is => 'rw' );
has colon      => ( is => 'ro', isa => 'Bool' );
has at         => ( is => 'ro', isa => 'Bool' );

=head1 NAME

JGoff::Lisp::Format::Tokens::R - Internal token for parser

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 METHODS

=cut

# {{{ _old_roman_numeral
sub _old_roman_numeral {
  my $self = shift;
  my ( $argument ) = @_;
  my ( $n_m, $n_d, $n_c, $n_l,
       $n_M, $n_D, $n_C, $n_L, $n_X, $n_V, $n_I ) =
     ( 0,    0,    0,    0, 
       0,    0,    0,    0,    0,    0,    0 );

  while ( $argument >= 100000 ) { $n_m++; $argument -= 100000 }
  while ( $argument >= 50000  ) { $n_d++; $argument -= 50000  }
  while ( $argument >= 10000  ) { $n_c++; $argument -= 10000  }
  while ( $argument >= 5000   ) { $n_l++; $argument -= 5000   }
  while ( $argument >= 1000   ) { $n_M++; $argument -= 1000   }
  while ( $argument >= 500    ) { $n_D++; $argument -= 500    }
  while ( $argument >= 100    ) { $n_C++; $argument -= 100    }
  while ( $argument >= 50     ) { $n_L++; $argument -= 50     }
  while ( $argument >= 10     ) { $n_X++; $argument -= 10     }
  while ( $argument >= 5      ) { $n_V++; $argument -= 5      }
  $n_I = $argument;

         # ROMAN NUMERAL ONE HUNDRED THOUSAND
  return ( $n_m ? "\x{2188}" x $n_m : '' ) .
         ( $n_d ? "\x{2187}" x $n_d : '' ) . # ROMAN NUMERAL FIFTY THOUSAND
         ( $n_c ? "\x{2182}" x $n_c : '' ) . # ROMAN NUMERAL TEN THOUSAND
         ( $n_l ? "\x{2181}" x $n_l : '' ) . # ROMAN NUMERAL FIVE THOUSAND
         ( $n_M ? 'M' x $n_M : '' ) .
         ( $n_D ? 'D' x $n_D : '' ) .
         ( $n_C ? 'C' x $n_C : '' ) .
         ( $n_L ? 'L' x $n_L : '' ) .
         ( $n_X ? 'X' x $n_X : '' ) .
         ( $n_V ? 'V' x $n_V : '' ) .
         ( $n_I ? 'I' x $n_I : '' );
}
# }}}

# {{{ _english_cardinal_number
sub _english_cardinal_number {
  my $self = shift;
  my ( $argument ) = @_;

  my @english_names = (
    "zero",          "one",           "two",           "three",
    "four",          "five",          "six",           "seven",
    "eight",         "nine",          "ten",           "eleven",
    "twelve",        "thirteen",      "fourteen",      "fifteen",
    "sixteen",       "seventeen",     "eighteen",      "nineteen",
    "twenty",        "twenty-one",    "twenty-two",    "twenty-three",
    "twenty-four",   "twenty-five",   "twenty-six",    "twenty-seven",
    "twenty-eight",  "twenty-nine",   "thirty",        "thirty-one",
    "thirty-two",    "thirty-three",  "thirty-four",   "thirty-five",
    "thirty-six",    "thirty-seven",  "thirty-eight",  "thirty-nine",
    "forty",         "forty-one",     "forty-two",     "forty-three",
    "forty-four",    "forty-five",    "forty-six",     "forty-seven",
    "forty-eight",   "forty-nine",    "fifty",         "fifty-one",
    "fifty-two",     "fifty-three",   "fifty-four",    "fifty-five",
    "fifty-six",     "fifty-seven",   "fifty-eight",   "fifty-nine",
    "sixty",         "sixty-one",     "sixty-two",     "sixty-three",
    "sixty-four",    "sixty-five",    "sixty-six",     "sixty-seven",
    "sixty-eight",   "sixty-nine",    "seventy",       "seventy-one",
    "seventy-two",   "seventy-three", "seventy-four",  "seventy-five",
    "seventy-six",   "seventy-seven", "seventy-eight", "seventy-nine",
    "eighty",        "eighty-one",    "eighty-two",    "eighty-three",
    "eighty-four",   "eighty-five",   "eighty-six",    "eighty-seven",
    "eighty-eight",  "eighty-nine",   "ninety",        "ninety-one",
    "ninety-two",    "ninety-three",  "ninety-four",   "ninety-five",
    "ninety-six",    "ninety-seven",  "ninety-eight",  "ninety-nine",
    "one hundred"
  );
  my @thousand = (
    "thousand", # 1_000
    "million", # 1_000_000
    "billion", # 1_000_000_000
    "trillion", # 1_000_000_000_000
  );
  my $english = $english_names[ abs( $argument ) ];
  $english = 'negative ' . $english if $argument < 0;
  return $english;
}
# }}}

# {{{ _english_ordinal_number
sub _english_ordinal_number {
  my $self = shift;
  my ( $argument ) = @_;

  my @english_names = (
    "zeroth",         "first",           "second",         "third",
    "fourth",         "fifth",           "sixth",          "seventh",
    "eighth",         "ninth",           "tenth",          "eleventh",
    "twelfth",        "thirteenth",      "fourteenth",     "fifteenth",
    "sixteenth",      "seventeenth",     "eighteenth",     "nineteenth",
    "twentieth",      "twenty-first",    "twenty-second",  "twenty-third",
    "twenty-fourth",  "twenty-fifth",    "twenty-sixth",   "twenty-seventh",
    "twenty-eighth",  "twenty-ninth",    "thirtieth",      "thirty-first",
    "thirty-second",  "thirty-third",    "thirty-fourth",  "thirty-fifth",
    "thirty-sixth",   "thirty-seventh",  "thirty-eighth",  "thirty-ninth",
    "fortieth",       "forty-first",     "forty-second",   "forty-third",
    "forty-fourth",   "forty-fifth",     "forty-sixth",    "forty-seventh",
    "forty-eighth",   "forty-ninth",     "fiftieth",       "fifty-first",
    "fifty-second",   "fifty-third",     "fifty-fourth",   "fifty-fifth",
    "fifty-sixth",    "fifty-seventh",   "fifty-eighth",   "fifty-ninth",
    "sixtieth",       "sixty-first",     "sixty-second",   "sixty-third",
    "sixty-fourth",   "sixty-fifth",     "sixty-sixth",    "sixty-seventh",
    "sixty-eighth",   "sixty-ninth",     "seventieth",     "seventy-first",
    "seventy-second", "seventy-third",   "seventy-fourth", "seventy-fifth",
    "seventy-sixth",  "seventy-seventh", "seventy-eighth", "seventy-ninth",
    "eightieth",      "eighty-first",    "eighty-second",  "eighty-third",
    "eighty-fourth",  "eighty-fifth",    "eighty-sixth",   "eighty-seventh",
    "eighty-eighth",  "eighty-ninth",    "ninetieth",      "ninety-first",
    "ninety-second",  "ninety-third",    "ninety-fourth",  "ninety-fifth",
    "ninety-sixth",   "ninety-seventh",  "ninety-eighth",  "ninety-ninth",
    "one hundredth"
  );
  my @thousand = (
    "thousand", # 1_000
    "million", # 1_000_000
    "billion", # 1_000_000_000
    "trillion", # 1_000_000_000_000
  );
  my $english = $english_names[ abs( $argument ) ];
  $english = 'negative ' . $english if $argument < 0;
  return $english;
}
# }}}

=head2 format( $core )

=cut

sub format {
  my $self = shift;
  my ( $core ) = @_;
  my $has_arguments = $self->parameters and
                      @{ $self->parameters } ? 1 : undef;
  $self->_resolve_parameters(
    $core, [
      [ 'radix' => undef ],
      [ 'mincol' => 0 ],
      [ 'padchar' => ' ' ],
#      [ 'colinc' => 1 ],
      [ 'commachar' => ',' ],
      [ 'comma-interval' => 3 ],
    ]
  );
$self->{colinc} = 1;

  my $argument = $core->current_argument;
  if ( $self->{radix} and
       ( $self->{radix} != 10 or
         $self->{'radix-v'} or
         ( !defined $argument and
           $self->{'radix-v'} ) ) ) {
    $argument = $self->_argument_to_base( $self->{radix}, $argument );
  }
  elsif ( $self->at and
          $self->colon ) {
    $argument = $self->_old_roman_numeral( $argument );
  }
  elsif ( $self->{radix} and
          $self->{radix} == 10 ) {
    $argument = $self->_commify( $argument );
  }
  elsif ( $self->colon ) {
      $argument = $self->_english_ordinal_number( $argument );
  }
  elsif ( $self->at ) {
    $argument = $self->_old_roman_numeral( $argument );
  }
  else {
    $argument = $self->_english_cardinal_number( $argument );
  }

  return $argument;
}

1;
