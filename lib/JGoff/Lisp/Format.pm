package JGoff::Lisp::Format;

use Moose;

use Readonly;

use JGoff::Lisp::Format::Parser;
use Carp qw( croak );
use POSIX qw( abs );
use Storable qw( dclone );

Readonly our $upcase => 'upcase';
Readonly our $downcase => 'downcase';
Readonly our $capitalize => 'capitalize';

our $print_case = $upcase; # default value from the CLISP spec

Readonly our $most_positive_fixnum => 2**32-1;#~0; # XXX Probably wrong
Readonly our $most_negative_fixnum => -(2**32-1);#~0; # XXX Probably wrong

has parser => (
  is => 'rw',
  isa => 'JGoff::Lisp::Format::Parser',
  default => sub {
    JGoff::Lisp::Format::Parser->new( patterns => { ws => undef } );
  }
);

has arguments => ( is => 'rw' );
has argument_id => ( is => 'rw', isa => 'Int', default => 0 );

sub inc_argument_id { $_[0]->argument_id( $_[0]->argument_id + 1 ) }

sub current_argument { $_[0]->arguments->[ $_[0]->arguemnt_id ] }

sub next_arguemnt {
  my $self = shift;
  $self->inc_argument_id;
  $self->current_argument;
}

=head1 NAME

JGoff::Lisp::Format - The great new JGoff::Lisp::Format!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

Quick summary of what the module does.

Perhaps a little code snippet.

    use JGoff::Lisp::Format;

    my $foo = JGoff::Lisp::Format->new();
    ...

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.

=head1 SUBROUTINES/METHODS

=head2 format( $stream, $format, @args )

=cut

# {{{ char_name

sub char_name {
  my $self = shift;
  my ( $char ) = @_;
  return 'Space' if $char eq ' ';
  return $char;
}

# }}}

# {{{ _print_case( $argument )

sub _print_case {
  my $self = shift;
  my ( $argument ) = @_;
  if ( $print_case eq $upcase ) {
    if ( ref( $argument ) and
         ref( $argument ) eq 'JGoff::Lisp::Format::Utils::Character' ) { # XXX
      return $argument;
    }
    return uc( $argument );
  }
  elsif ( $print_case eq $downcase ) {
    return lc( $argument );
  }
  elsif ( $print_case eq $capitalize ) {
    return ucfirst( lc( $argument ) );
  }
  else {
    croak "Unknown or missing print_case '$print_case'";
  }
}

# }}}

# {{{ _commify( $argument, $interval, $commachar )

sub _commify {
  my $self = shift;
  my ( $argument, $interval, $commachar ) = @_;

  my @chunk;
  while ( $argument and
          length( $argument ) > $interval ) {
    unshift @chunk, substr( $argument, -$interval, $interval, '' );
  }
  unshift @chunk, $argument if $argument and $argument ne '';
  $argument = join $commachar, @chunk;
  return $argument;
}

# }}}

# {{{ _padding ( $element, $argument )

sub _padding {
  my ( $self, $element, $argument ) = @_;
  my ( $padchar, $minpad, $mincol, $colinc ) =
    @{ $element }{qw( padchar minpad mincol colinc )};

  my $padding = '';
  if ( $minpad > 0 ) {
    $padding .= $padchar x $minpad;
  }
  if ( $mincol and $mincol > length( $argument ) ) {
    if ( $colinc and $colinc > 0 ) {
      while ( length( $argument ) + length( $padding ) < $mincol ) {
        $padding .= $padchar x $colinc;
      }
    }
  }

  if ( $element->{at} ) {
    $argument = $padding . $argument;
  }
  else {
    $argument = $argument . $padding;
  }
}

# }}}

# {{{ __format_a

sub __format_a {
  my $self = shift;
  my ( $element ) = @_;
  my $arguments = $self->arguments;

  @{ $element }{qw( mincol minpad colinc padchar )} =
    ( 0, 0, 1, ' ' );

  if ( $element->{arguments} ) {
    my $mincol = shift @{ $element->{arguments} };
    my $colinc = shift @{ $element->{arguments} };
    my $minpad = shift @{ $element->{arguments} };
    my $padchar = shift @{ $element->{arguments} };

    $element->{mincol} = $mincol if defined $mincol;
    $element->{colinc} = $colinc if defined $colinc;
    $element->{minpad} = $minpad if defined $minpad;
    $element->{padchar} = $padchar if defined $padchar;
  }
  delete $element->{arguments};   

  for my $arg ( qw( mincol colinc minpad padchar ) ) {
    if ( $element->{$arg} and $element->{$arg} eq 'v' ) {
      $element->{$arg} = shift @{ $arguments };
    }
    elsif ( $element->{$arg} and $element->{$arg} eq '#' ) {
      $element->{$arg} = scalar @{ $arguments };
    }
  }
  $element->{minpad} = 0 unless defined $element->{minpad};
  $element->{colinc} = 1 unless defined $element->{colinc};
  $element->{padchar} = ' ' unless defined $element->{padchar};
  $element->{padchar} =~ s{^'}{};

  my $argument;
  if ( $arguments and ref( $arguments ) and ref( $arguments ) eq 'ARRAY' ) {
    $argument = shift @{ $arguments };
  }

# Strip escape characters

  if ( !defined $argument ) {
    if ( $element->{colon} ) {
      return '[]';
    }
    $argument = $self->_padding( $element, 'undef' );
    return $self->_print_case( $argument );
  }
  elsif ( ref( $argument ) and ref( $argument ) eq 'ARRAY' ) {
   return '[' .
          $self->format( undef, $element->{format}, @{ $argument } ) .
          ']';
  }
  elsif ( ref( $argument ) and ref( $argument ) =~ /Character/ ) {
    return $argument->toString;
  }
  else {
    $argument = $self->_padding( $element, $argument );
    return $argument;
  }
  
  return $argument;
}

# }}}

# {{{ __format_ampersand

sub __format_ampersand {
  my $self = shift;
  my ( $element , $arguments, $is_not_first ) = @_;
  $element->{n} = 0;
  if ( $element->{arguments} ) {
    my $n = shift @{ $element->{arguments} };

    $element->{n} = $n if defined $n;
  }
  delete $element->{arguments};   

  if ( $element->{n} and $element->{n} eq 'v' ) {
    $element->{n} = shift @{ $arguments };
  }
  elsif ( $element->{n} and $element->{n} eq '#' ) {
    $element->{n} = scalar @{ $arguments };
  }
  $element->{n} = 0 unless defined $element->{n};

  my $argument = shift @{ $arguments };

  if ( $is_not_first ) {
    return "\n";
  }

  return "\n" x $element->{n};
}

# }}}

# {{{ __format_b

sub __format_b {
  my $self = shift;
  my ( $element , $arguments ) = @_;
  @{ $element }{qw( mincol padchar commachar comma-interval )} =
    ( 0, ' ', ',', 3 );
  if ( $element->{arguments} ) {
    my $mincol = shift @{ $element->{arguments} };
    my $padchar = shift @{ $element->{arguments} };
    my $commachar = shift @{ $element->{arguments} };
    my $comma_interval = shift @{ $element->{arguments} };

    $element->{mincol} = $mincol if defined $mincol;
    $element->{padchar} = $padchar if defined $padchar;
    $element->{commachar} = $commachar if defined $commachar;
    $element->{'comma-interval'} = $comma_interval if defined $comma_interval;
  }
  delete $element->{arguments};   

  for my $arg ( qw( mincol padchar commachar comma-interval ) ) {
    if ( $element->{$arg} and $element->{$arg} eq 'v' ) {
      $element->{$arg} = shift @{ $arguments };
    }
    elsif ( $element->{$arg} and $element->{$arg} eq '#' ) {
      $element->{$arg} = scalar @{ $arguments };
    }
  }
  $element->{mincol} = 0 unless defined $element->{mincol};
  $element->{minpad} = 0 unless defined $element->{minpad};
  $element->{padchar} = ' ' unless defined $element->{padchar};
  $element->{commachar} = ',' unless defined $element->{commachar};
  $element->{'comma-interval'} = 3 unless defined $element->{'comma-interval'};
  $element->{padchar} =~ s{^'(.)}{$1};
  $element->{commachar} =~ s{^'(.)}{$1};

  my $argument = shift @{ $arguments };
  my %binary = (
    0  => '0',
    1  => '1',
  );

  my $bits = '';
  my $sign = $argument < 0 ? -1 : 1;
  $argument = abs( $argument );

  while ( $argument > 0 ) {
    my $bit = $argument % 2;
    $bits = $binary{$bit} . $bits;
    $argument -= $bit;
    $argument /= 2;
  }

  if ( $element->{colon} ) {
    $bits = $self->_commify(
      $bits, $element->{'comma-interval'}, $element->{commachar}
    );
  }
  $bits = '-' . $bits if $sign == -1;
  $bits = '+' . $bits if $sign == +1 and $element->{at};

  if ( $bits and $element->{mincol} > 0 and length( $bits ) < $element->{mincol} ) {
    $bits = ' ' x ( $element->{mincol} - length( $bits ) ) . $bits;
  }

  return $bits;
}

# }}}

# {{{ __format_c

sub __format_c {
  my $self = shift;
  my ( $element, $arguments ) = @_;

  my $argument = shift @{ $arguments };
  if ( $element->{colon} ) {
    return $self->char_name( $argument );
  }
  return $argument;
}

# }}}

# {{{ __format_d

sub __format_d {
  my $self = shift;
  my ( $element , $arguments ) = @_;
  @{ $element }{qw( mincol padchar commachar comma-interval )} =
    ( 0, ' ', ',', 3 );
  if ( $element->{arguments} ) {
    my $mincol = shift @{ $element->{arguments} };
    my $padchar = shift @{ $element->{arguments} };
    my $commachar = shift @{ $element->{arguments} };
    my $comma_interval = shift @{ $element->{arguments} };

    $element->{mincol} = $mincol if defined $mincol;
    $element->{padchar} = $padchar if defined $padchar;
    $element->{commachar} = $commachar if defined $commachar;
    $element->{'comma-interval'} = $comma_interval if defined $comma_interval;
  }
  delete $element->{arguments};   

  for my $arg ( qw( mincol padchar commachar comma-interval ) ) {
    if ( $element->{$arg} and $element->{$arg} eq 'v' ) {
      $element->{$arg} = shift @{ $arguments };
    }
    elsif ( $element->{$arg} and $element->{$arg} eq '#' ) {
      $element->{$arg} = scalar @{ $arguments };
    }
  }
  $element->{mincol} = 0 unless defined $element->{mincol};
  $element->{minpad} = 0 unless defined $element->{minpad};
  $element->{padchar} = ' ' unless defined $element->{padchar};
  $element->{commachar} = ',' unless defined $element->{commachar};
  $element->{'comma-interval'} = 3 unless defined $element->{'comma-interval'};
  $element->{padchar} =~ s{^'(.)}{$1};
  $element->{commachar} =~ s{^'(.)}{$1};

  my $argument = shift @{ $arguments };
  my $sign = 1;

  if ( $argument and $argument < 0 ) {
    $sign = -1;
    $argument = abs( $argument );
  }

  if ( $element->{colon} ) {
    $argument = $self->_commify(
      $argument, $element->{'comma-interval'}, $element->{commachar}
    );
  }
  $argument = '-' . $argument if $sign == -1;
  $argument = '+' . $argument if $sign == +1 and $element->{at};

  if ( $argument and
       $element->{mincol} > 0 and
       length( $argument ) < $element->{mincol} ) {
    $argument = ' ' x ( $element->{mincol} - length( $argument ) ) . $argument;
  }

  return $argument;
}

# }}}

# {{{ __format_f

sub __format_f {
  my $self = shift;
  my ( $element , $arguments ) = @_;
#  @{ $element }{qw( w d k overflowchar padchar )} =
#    ( 0, ' ', ',', 3 );
  if ( $element->{arguments} ) {
    my $w = shift @{ $element->{arguments} };
    my $d = shift @{ $element->{arguments} };
    my $k = shift @{ $element->{arguments} };
    my $overflowchar = shift @{ $element->{arguments} };
    my $padchar = shift @{ $element->{arguments} };

    $element->{w} = $w if defined $w;
    $element->{d} = $d if defined $d;
    $element->{k} = $k if defined $k;
    $element->{overflowchar} = $overflowchar if defined $overflowchar;
    $element->{padchar} = $padchar if defined $padchar;
  }
  delete $element->{arguments};   

  for my $arg ( qw( w d k overflowchar padchar ) ) {
    if ( $element->{$arg} and $element->{$arg} eq 'v' ) {
      $element->{$arg} = shift @{ $arguments };
    }
    elsif ( $element->{$arg} and $element->{$arg} eq '#' ) {
      $element->{$arg} = scalar @{ $arguments };
    }
  }
  $element->{w} = 0 unless defined $element->{w};
  $element->{d} = 0 unless defined $element->{d};
  $element->{k} = 0 unless defined $element->{k};
  $element->{overflowchar} = ',' unless defined $element->{overflowchar};
  $element->{padchar} = ',' unless defined $element->{padchar};
  $element->{overflowchar} =~ s{^'(.)}{$1};
  $element->{padchar} =~ s{^'(.)}{$1};

  my $argument = shift @{ $arguments };
  my $sign = 1;

  if ( $argument and $argument < 0 ) {
    $sign = -1;
    $argument = abs( $argument );
  }
  $argument = sprintf "%f", $argument;
  if ( $argument =~ m{ [.] [0]+ $ }x ) {
    $argument =~ s{ [.] [0]+ $ }{.0}x;
  }

  if ( $element->{colon} ) {
    $argument = $self->_commify(
      $argument, $element->{'comma-interval'}, $element->{commachar}
    );
  }
  $argument = '-' . $argument if $sign == -1;
  $argument = '+' . $argument if $sign == +1 and $element->{at};

  if ( $argument and
       $element->{w} > 0 and
       length( $argument ) < $element->{w} ) {
    $argument = ' ' x ( $element->{w} - length( $argument ) ) . $argument;
  }

  return $argument;
}

# }}}

# {{{ __format_o

sub __format_o {
  my $self = shift;
  my ( $element , $arguments ) = @_;
  @{ $element }{qw( mincol padchar commachar comma-interval )} =
    ( 0, ' ', ',', 3 );
  if ( $element->{arguments} ) {
    my $mincol = shift @{ $element->{arguments} };
    my $padchar = shift @{ $element->{arguments} };
    my $commachar = shift @{ $element->{arguments} };
    my $comma_interval = shift @{ $element->{arguments} };

    $element->{mincol} = $mincol if defined $mincol;
    $element->{padchar} = $padchar if defined $padchar;
    $element->{commachar} = $commachar if defined $commachar;
    $element->{'comma-interval'} = $comma_interval if defined $comma_interval;
  }
  delete $element->{arguments};   

  for my $arg ( qw( mincol padchar commachar comma-interval ) ) {
    if ( $element->{$arg} and $element->{$arg} eq 'v' ) {
      $element->{$arg} = shift @{ $arguments };
    }
    elsif ( $element->{$arg} and $element->{$arg} eq '#' ) {
      $element->{$arg} = scalar @{ $arguments };
    }
  }
  $element->{mincol} = 0 unless defined $element->{mincol};
  $element->{minpad} = 0 unless defined $element->{minpad};
  $element->{padchar} = ' ' unless defined $element->{padchar};
  $element->{commachar} = ',' unless defined $element->{commachar};
  $element->{'comma-interval'} = 3 unless defined $element->{'comma-interval'};
  $element->{padchar} =~ s{^'(.)}{$1};
  $element->{commachar} =~ s{^'(.)}{$1};

  my $argument = shift @{ $arguments };
  my %octal = (
    0  => '0',
    1  => '1',
    2  => '2',
    3  => '3',
    4  => '4',
    5  => '5',
    6  => '6',
    7  => '7',
  );

  my $bits = '';
  my $sign = $argument < 0 ? -1 : 1;
  $argument = abs( $argument );

  while ( $argument > 0 ) {
    my $bit = $argument % 8;
    $bits = $octal{$bit} . $bits;
    $argument -= $bit;
    $argument /= 8;
  }

  if ( $element->{colon} ) {
    $bits = $self->_commify(
      $bits, $element->{'comma-interval'}, $element->{commachar}
    );
  }
  $bits = '-' . $bits if $sign == -1;
  $bits = '+' . $bits if $sign == +1 and $element->{at};

  if ( $bits and $element->{mincol} > 0 and length( $bits ) < $element->{mincol} ) {
    $bits = ' ' x ( $element->{mincol} - length( $bits ) ) . $bits;
  }

  return $bits;
}

# }}}

# {{{ __format_p

sub __format_p {
  my $self = shift;
  my ( $element, $arguments ) = @_;

  my $argument = shift @{ $arguments };
  $argument = 0 unless defined $argument;

if ( $argument eq 'No' ) {
  if ( $element->{at} and $element->{colon} ) {
return '-A';
  }
  elsif ( $element->{colon} ) {
return '-B';
  }
  elsif ( $element->{at} ) {
return '-C';
  }
  else {
return '-D';
  }
}
elsif ( $argument == 0 ) {
  if ( $element->{at} and $element->{colon} ) {
return 'y';
return 'A';
  }
  elsif ( $element->{colon} ) {
return '';
return 'B';
  }
  elsif ( $element->{at} ) {
return 'ies';
return 'C';
  }
  else {
return 's';
return 'D';
  }
}
elsif ( $argument == 1 ) {
  if ( $element->{at} and $element->{colon} ) {
return 'E';
  }
  elsif ( $element->{colon} ) {
return 'F';
  }
  elsif ( $element->{at} ) {
return '';
return 'G';
  }
  else {
return '';
return 'H';
  }
}
elsif ( $argument >= 2 ) {
  if ( $element->{at} and $element->{colon} ) {
return 'I';
  }
  elsif ( $element->{colon} ) {
return 'J';
  }
  elsif ( $element->{at} ) {
return 'ies';
return 'K';
  }
  else {
return 's';
return 'L';
  }
}
else {
  if ( $element->{at} and $element->{colon} ) {
return 'M';
  }
  elsif ( $element->{colon} ) {
return 'N';
  }
  elsif ( $element->{at} ) {
return 'O';
  }
  else {
return 'P';
  }
}

  return '';
}

# }}}

# {{{ __format_x

sub __format_x {
  my $self = shift;
  my ( $element , $arguments ) = @_;
  @{ $element }{qw( mincol padchar commachar comma-interval )} =
    ( 0, ' ', ',', 3 );
  if ( $element->{arguments} ) {
    my $mincol = shift @{ $element->{arguments} };
    my $padchar = shift @{ $element->{arguments} };
    my $commachar = shift @{ $element->{arguments} };
    my $comma_interval = shift @{ $element->{arguments} };

    $element->{mincol} = $mincol if defined $mincol;
    $element->{padchar} = $padchar if defined $padchar;
    $element->{commachar} = $commachar if defined $commachar;
    $element->{'comma-interval'} = $comma_interval if defined $comma_interval;
  }
  delete $element->{arguments};   

  for my $arg ( qw( mincol padchar commachar comma-interval ) ) {
    if ( $element->{$arg} and $element->{$arg} eq 'v' ) {
      $element->{$arg} = shift @{ $arguments };
    }
    elsif ( $element->{$arg} and $element->{$arg} eq '#' ) {
      $element->{$arg} = scalar @{ $arguments };
    }
  }
  $element->{mincol} = 0 unless defined $element->{mincol};
  $element->{minpad} = 0 unless defined $element->{minpad};
  $element->{padchar} = ' ' unless defined $element->{padchar};
  $element->{commachar} = ',' unless defined $element->{commachar};
  $element->{'comma-interval'} = 3 unless defined $element->{'comma-interval'};
  $element->{padchar} =~ s{^'(.)}{$1};
  $element->{commachar} =~ s{^'(.)}{$1};

  my $argument = shift @{ $arguments };
  my %hexadecimal = (
    0  => '0',
    1  => '1',
    2  => '2',
    3  => '3',
    4  => '4',
    5  => '5',
    6  => '6',
    7  => '7',
    8  => '8',
    9  => '9',
    10 => 'a',
    11 => 'b',
    12 => 'c',
    13 => 'd',
    14 => 'e',
    15 => 'f',
  );

  my $bits = '';
  my $sign = $argument < 0 ? -1 : 1;
  $argument = abs( $argument );

  while ( $argument > 0 ) {
    my $bit = $argument % 16;
    $bits = $hexadecimal{$bit} . $bits;
    $argument -= $bit;
    $argument /= 16;
  }

  if ( $element->{colon} ) {
    $bits = $self->_commify(
      $bits, $element->{'comma-interval'}, $element->{commachar}
    );
  }
  $bits = '-' . $bits if $sign == -1;
  $bits = '+' . $bits if $sign == +1 and $element->{at};

  if ( $bits and $element->{mincol} > 0 and length( $bits ) < $element->{mincol} ) {
    $bits = ' ' x ( $element->{mincol} - length( $bits ) ) . $bits;
  }

  return $bits;
}

# }}}

# {{{ __format_newline

sub __format_newline {
  my $self = shift;
  my ( $element , $arguments ) = @_;
  $element->{n} = 1;
  if ( $element->{arguments} ) {
    my $n = shift @{ $element->{arguments} };

    $element->{n} = $n if defined $n;
  }
  delete $element->{arguments};   

  if ( $element->{n} and $element->{n} eq 'v' ) {
    $element->{n} = shift @{ $arguments };
  }
  elsif ( $element->{n} and $element->{n} eq '#' ) {
    $element->{n} = defined $arguments ? scalar @{ $arguments } : 0;
  }
  $element->{n} = 0 unless defined $element->{n};

  return "\n" x $element->{n};
}

# }}}

# {{{ __format_percent

sub __format_percent {
  my $self = shift;
  my ( $element , $arguments ) = @_;
  $element->{n} = 0;
  if ( $element->{arguments} ) {
    my $n = shift @{ $element->{arguments} };

    $element->{n} = $n if defined $n;
  }
  delete $element->{arguments};   

  if ( $element->{n} and $element->{n} eq 'v' ) {
    $element->{n} = shift @{ $arguments };
  }
  elsif ( $element->{n} and $element->{n} eq '#' ) {
    $element->{n} = defined $arguments ? scalar @{ $arguments } : 0;
  }
  $element->{n} = 0 unless defined $element->{n};

  return "\n" x $element->{n};
}

# }}}

# {{{ __format_open_brace

sub __format_open_brace {
  my $self = shift;
  my ( $open, $element, $close , $arguments ) = @_;
  my $max_iterations;

  my $output = '';
my $count = 10;
  if ( $arguments and ref( $arguments ) eq 'ARRAY' and @{ $arguments } ) {
    while ( @{ $arguments } ) {
      last if defined $max_iterations and $max_iterations-- <= 0;
if ( $count-- < 0 ) {
  return 'OPEN BRACE';
  last;
}
    }
    $output .= $self->_format( $element, $arguments );
  }
  return $output;
}

# }}}

# {{{ __format_tilde

sub __format_tilde {
  my $self = shift;
  my ( $element , $arguments ) = @_;
  $element->{n} = 1;
  if ( $element->{arguments} ) {
    my $n = shift @{ $element->{arguments} };

    $element->{n} = $n if defined $n;
  }
  delete $element->{arguments};   

  if ( $element->{n} and $element->{n} eq 'v' ) {
    $element->{n} = shift @{ $arguments };
  }
  elsif ( $element->{n} and $element->{n} eq '#' ) {
    $element->{n} = defined $arguments ? scalar @{ $arguments } : 0;
  }
  $element->{n} = 0 unless defined $element->{n};

  return "~" x $element->{n};
}

# }}}

# {{{ __format_vertical_bar

sub __format_vertical_bar {
  my $self = shift;
  my ( $element , $arguments ) = @_;
  $element->{n} = 1;
  if ( $element->{arguments} ) {
    my $n = shift @{ $element->{arguments} };

    $element->{n} = $n if defined $n;
  }
  delete $element->{arguments};   

  if ( $element->{n} and $element->{n} eq 'v' ) {
    $element->{n} = shift @{ $arguments };
  }
  elsif ( $element->{n} and $element->{n} eq '#' ) {
    $element->{n} = defined $arguments ? scalar @{ $arguments } : 0;
  }
  $element->{n} = 1 unless defined $element->{n};

  return "\cL" x $element->{n};
}

# }}}

# {{{ _format

sub _format {
  my $self = shift;
  my ( $tree ) = @_;
  my $arguments = $self->arguments;
  my $output;
  for my $id ( 0 .. $#{ $tree } ) {
    my $element = $tree->[ $id ];
    if( ref( $element ) and ref( $element ) eq 'HASH' ) {
      if ( $element->{format} eq '~a' ) {
        $output .= $self->__format_a( $element, $arguments );
      }
      elsif ( $element->{format} eq '~&' ) {
        $output .= $self->__format_ampersand(
          $element, $arguments, ( $id > 0 )
        );
      }
      elsif ( $element->{format} eq '~b' ) {
        $output .= $self->__format_b( $element, $arguments );
      }
      elsif ( $element->{format} eq '~c' ) {
        $output .= $self->__format_c( $element, $arguments );
      }
      elsif ( $element->{format} eq '~d' ) {
        $output .= $self->__format_d( $element, $arguments );
      }
      elsif ( $element->{format} eq '~f' ) {
        $output .= $self->__format_f( $element, $arguments );
      }
      elsif ( $element->{format} eq '~\n' ) {
        $output .= $self->__format_newline( $element, $arguments );
      }
      elsif ( $element->{format} eq '~o' ) {
        $output .= $self->__format_o( $element, $arguments );
      }
      elsif ( $element->{format} eq '~p' ) {
        $output .= $self->__format_p( $element, $arguments );
      }
      elsif ( $element->{format} eq '~%' ) {
        $output .= $self->__format_percent( $element, $arguments );
      }
      elsif ( $element->{format} eq '~~' ) {
        $output .= $self->__format_tilde( $element, $arguments );
      }
      elsif ( $element->{format} eq '~x' ) {
        $output .= $self->__format_x( $element, $arguments );
      }
      elsif ( $element->{format} eq '~|' ) {
        $output .= $self->__format_vertical_bar( $element, $arguments );
      }
      else {
        $output = 'UNIMPLEMENTED FORMAT'; last;
      }
    }
    elsif ( ref( $element ) and ref( $element ) eq 'ARRAY' ) {
      my ( $open, $_element, $close ) = @{ $element };
      my $_arguments;
      if ( $arguments and ref( $arguments ) eq 'ARRAY' ) {
        $_arguments = $arguments->[0];
      }
      if ( $open->{format} eq '~{' ) {
        $output .= $self->__format_open_brace(
          $open, $_element, $close, $_arguments
        );
      }
      elsif ( $open->{format} eq '~(' ) {
        $output .= $self->__format_open_paren(
          $open, $_element, $close, $_arguments
        );
      }
      elsif ( $open->{format} eq '~[' ) {
        $output .= $self->__format_open_bracket(
          $open, $_element, $close, $_arguments
        );
      }
    }
    else {
      $output .= $element;
    }

  }
  return $output;
}

# }}}

# {{{ format

sub format {
  my $self = shift;
  my ( $stream, $format, $arguments ) = @_;

  if ( my $tree = $self->parser->from_string( $format ) ) {
    $self->arguments( $arguments );
    return $self->_format( $tree );
    $self->arguments( undef );
  }
  
  return 'Not Caught';
}

# }}}

=head2 formatter

=cut

# {{{ formatter

sub formatter {
  my $self = shift;
  my ( $format ) = @_;

  return sub {
    my ( $stream, $args ) = @_;
    return $self->format( $stream, $format, $args );
  };
}

# }}}

=head1 AUTHOR

Jeff Goff, C<< <jgoff at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-jgoff-lisp-format at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=JGoff-Lisp-Format>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc JGoff::Lisp::Format


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=JGoff-Lisp-Format>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/JGoff-Lisp-Format>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/JGoff-Lisp-Format>

=item * Search CPAN

L<http://search.cpan.org/dist/JGoff-Lisp-Format/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2012 Jeff Goff.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1; # End of JGoff::Lisp::Format
