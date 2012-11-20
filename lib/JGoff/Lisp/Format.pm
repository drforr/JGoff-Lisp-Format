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
  default => sub{
    JGoff::Lisp::Format::Parser->new( patterns => { ws => undef } );
  }
);

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
  my ( $element , $arguments ) = @_;
  $element->{mincol} = 0;
  $element->{minpad} = 0;
  $element->{colinc} = 1;
  $element->{padchar} = ' ';
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
    my @chunk;
    while ( $bits and length( $bits ) > $element->{'comma-interval'} ) {
      unshift @chunk, substr(
        $bits, -$element->{'comma-interval'}, $element->{'comma-interval'}, 
        ''
      );
    }
    unshift @chunk, $bits if $bits and $bits ne '';
    $bits = join $element->{commachar}, @chunk;
  }
  $bits = '-' . $bits if $sign == -1;
  $bits = '+' . $bits if $sign == +1 and $element->{at};

  if ( $bits and $element->{mincol} > 0 and length( $bits ) < $element->{mincol} ) {
    $bits = ' ' x ( $element->{mincol} - length( $bits ) ) . $bits;
  }

  return $bits;
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

  my $bits = '';
  my $sign = $argument < 0 ? -1 : 1;
  $argument = abs( $argument );

  $bits = $argument;

  if ( $element->{colon} ) {
    my @chunk;
    while ( $bits and length( $bits ) > $element->{'comma-interval'} ) {
      unshift @chunk, substr(
        $bits, -$element->{'comma-interval'}, $element->{'comma-interval'}, 
        ''
      );
    }
    unshift @chunk, $bits if $bits and $bits ne '';
    $bits = join $element->{commachar}, @chunk;
  }
  $bits = '-' . $bits if $sign == -1;
  $bits = '+' . $bits if $sign == +1 and $element->{at};

  if ( $bits and $element->{mincol} > 0 and length( $bits ) < $element->{mincol} ) {
    $bits = ' ' x ( $element->{mincol} - length( $bits ) ) . $bits;
  }

  return $bits;
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
    my @chunk;
    while ( $bits and length( $bits ) > $element->{'comma-interval'} ) {
      unshift @chunk, substr(
        $bits, -$element->{'comma-interval'}, $element->{'comma-interval'}, 
        ''
      );
    }
    unshift @chunk, $bits if $bits and $bits ne '';
    $bits = join $element->{commachar}, @chunk;
  }
  $bits = '-' . $bits if $sign == -1;
  $bits = '+' . $bits if $sign == +1 and $element->{at};

  if ( $bits and $element->{mincol} > 0 and length( $bits ) < $element->{mincol} ) {
    $bits = ' ' x ( $element->{mincol} - length( $bits ) ) . $bits;
  }

  return $bits;
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
    my @chunk;
    while ( $bits and length( $bits ) > $element->{'comma-interval'} ) {
      unshift @chunk, substr(
        $bits, -$element->{'comma-interval'}, $element->{'comma-interval'}, 
        ''
      );
    }
    unshift @chunk, $bits if $bits and $bits ne '';
    $bits = join $element->{commachar}, @chunk;
  }
  $bits = '-' . $bits if $sign == -1;
  $bits = '+' . $bits if $sign == +1 and $element->{at};

  if ( $bits and $element->{mincol} > 0 and length( $bits ) < $element->{mincol} ) {
    $bits = ' ' x ( $element->{mincol} - length( $bits ) ) . $bits;
  }

  return $bits;
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

  my $output;
my $count = 10;
  while ( @{ $arguments } ) {
    last if defined $max_iterations and $max_iterations-- <= 0;
if ( $count-- < 0 ) {
  warn "*** Tripped the ~{..~}' alamr.";
  last;
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
  my ( $tree, $arguments ) = @_;
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
      elsif ( $element->{format} eq '~d' ) {
        $output .= $self->__format_d( $element, $arguments );
      }
      elsif ( $element->{format} eq '~o' ) {
        $output .= $self->__format_o( $element, $arguments );
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
      if ( $arguments and ref( $arguments ) ) {
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
    return $self->_format( $tree, $arguments );
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
