package JGoff::Lisp::Format::Core;

use Moose;

use Readonly;

use JGoff::Lisp::Format::Parser;
use Carp qw( croak );
use POSIX qw( abs );
use Storable qw( dclone );

has stream => ( is => 'rw' );
has format => ( is => 'rw' );
has arguments => ( is => 'rw' );
has parser => (
  is => 'rw',
  isa => 'JGoff::Lisp::Format::Parser',
  default => sub {
    JGoff::Lisp::Format::Parser->new( patterns => { ws => undef } );
  }
);

Readonly our $upcase => 'upcase';
Readonly our $downcase => 'downcase';
Readonly our $capitalize => 'capitalize';

Readonly our $most_positive_fixnum => 2**32-1;#~0; # XXX Probably wrong
Readonly our $most_negative_fixnum => -(2**32-1);#~0; # XXX Probably wrong

has print_case => ( is => 'ro', default => $upcase );

has argument_id => ( is => 'rw', isa => 'Int', default => 0 );

sub this_argument {
  my $self = shift;
  if ( $self->arguments ) {
    return $self->arguments->[ $self->argument_id ];
  }
  return undef;
}

sub advance_argument {
  my $self = shift;
  if ( $self->arguments ) {
    my $argument = $self->arguments->[ $self->argument_id ];
    $self->argument_id( $self->argument_id + 1 );
    return $argument;
  }
  return undef;
}

sub previous_argument {
  my $self = shift;
  if ( $self->arguments ) {
    my $argument = $self->arguments->[ $self->argument_id - 1 ];
    return $argument;
  }
  return undef;
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
  if ( $self->print_case eq $upcase ) {
    if ( ref( $argument ) and
         ref( $argument ) eq 'JGoff::Lisp::Format::Utils::Character' ) { # XXX
      return $argument;
    }
    return uc( $argument );
  }
  elsif ( $self->print_case eq $downcase ) {
    return lc( $argument );
  }
  elsif ( $self->print_case eq $capitalize ) {
    return ucfirst( lc( $argument ) );
  }
  else {
    croak "Unknown or missing print_case '" . $self->print_case . "'";
  }
}

# }}}

# {{{ _commify( $argument, $operation )

sub _commify {
  my $self = shift;
  my ( $argument, $operation ) = @_;
  my $interval = $operation->{'comma-interval'};
  my $commachar = $operation->{commachar};
  my $sign = 1;

  if ( $argument and $argument !~ /[^-+0-9.]/ and $argument < 0 ) {
    $sign = -1;
    $argument = abs( $argument );
  }
  if ( $operation->{colon} ) {
    my @chunk;
    while ( $argument and
            length( $argument ) > $interval ) {
      unshift @chunk, substr( $argument, -$interval, $interval, '' );
    }
    unshift @chunk, $argument if $argument and $argument ne '';
    $argument = join $commachar, @chunk;
  }
  $argument = '-' . $argument if $sign == -1;
  $argument = '+' . $argument if $sign == +1 and $operation->{at};
  return $argument;
}

# }}}

# {{{ _padding ( $operation, $argument )

sub _padding {
  my ( $self, $operation, $argument ) = @_;
  my ( $padchar, $minpad, $mincol, $colinc ) =
    @{ $operation }{qw( padchar minpad mincol colinc )};

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

  if ( $operation->{at} ) {
    $argument = $padding . $argument;
  }
  else {
    $argument = $argument . $padding;
  }
}

# }}}

# {{{ _resolve_arguments( $operation, $tuples )

sub _resolve_arguments {
  my $self = shift;
  my ( $operation, $tuples ) = @_;

  if ( $operation->{arguments} ) {
    for my $tuple ( @$tuples ) {
      my ( $name, $default ) = @$tuple;
      my $value = shift @{ $operation->{arguments} };
      if ( defined $value ) {
        $operation->{$name} = $value;
      }
    }
    delete $operation->{arguments};   
  }
  for my $tuple ( @$tuples ) {
    my ( $name, $default ) = @$tuple;
    if ( $operation->{$name} and $operation->{$name} eq 'v' ) {
      $operation->{$name} = $self->advance_argument;
    }
    elsif ( $operation->{$name} and $operation->{$name} eq '#' ) {
      $operation->{$name} = scalar @{ $self->arguments };
    }
  }
  for my $tuple ( @$tuples ) {
    my ( $name, $default ) = @$tuple;
    if ( !defined( $operation->{$name} ) ) {
      $operation->{$name} = $default;
    }
    if ( $name =~ /char/ ) {
      $operation->{$name} =~ s{^'(.)}{$1};
    }
  }
}

# }}}

# {{{ __format_text

sub __format_text {
  my $self = shift;
  my ( $operation, $before_newline, $nl_colon ) = @_;
  my $text = $operation->{arguments}[0];
  $text = '' unless defined $text;

  if ( $before_newline and !$nl_colon ) {
    $text =~ s{ ^ \s+ }{}x;
  }
  return $text;
}

# }}}

# {{{ __format_a

sub __format_a {
  my $self = shift;
  my ( $operation ) = @_;
  $self->_resolve_arguments(
    $operation, [
      [ 'mincol' => 0 ],
      [ 'colinc' => 1 ],
      [ 'minpad' => 0 ],
      [ 'padchar' => ' ' ] ]
  );

  my $argument;
  if ( $self->arguments and
       ref( $self->arguments ) and
       ref( $self->arguments ) eq 'ARRAY' ) {
    $argument = $self->advance_argument;
  }

# Strip escape characters

  if ( !defined $argument ) {
    if ( $operation->{colon} ) {
      return '[]';
    }
    $argument = $self->_padding( $operation, 'undef' );
    return $self->_print_case( $argument );
  }
  elsif ( ref( $argument ) and ref( $argument ) eq 'ARRAY' ) {
   my $sub = $self->new(
     stream => $self->stream,
     format => $operation->{format},
     arguments => $argument,

     print_case => $self->print_case,
   );
   return '[' . $sub->apply . ']';
  }
  elsif ( ref( $argument ) and ref( $argument ) =~ /Character/ ) {
    return $argument->toString;
  }
  else {
    $argument = $self->_padding( $operation, $argument );
    return $argument;
  }
  
  return $argument;
}

# }}}

# {{{ __format_ampersand

sub __format_ampersand {
  my $self = shift;
  my ( $operation, $is_first, $before_percent ) = @_;
  if ( !$is_first and !$before_percent ) {
    return "\n";
  }

  return "";
}

# }}}

# {{{ __format_b

sub __format_b {
  my $self = shift;
  my ( $operation ) = @_;
  $self->_resolve_arguments(
    $operation, [
      [ 'mincol' => 0 ],
      [ 'padchar' => ' ' ],
      [ 'commachar' => ',' ],
      [ 'comma-interval' => 3 ],
    ]
  );

  my $argument = $self->advance_argument;
  my $bits = '';
  while ( $argument > 0 ) {
    my $bit = $argument % 2;
    $bits = $bit . $bits;
    $argument -= $bit;
    $argument /= 2;
  }

  $bits = $self->_commify( $bits, $operation );

  if ( $bits and $operation->{mincol} > 0 and length( $bits ) < $operation->{mincol} ) {
    $bits = ' ' x ( $operation->{mincol} - length( $bits ) ) . $bits;
  }

  return $bits;
}

# }}}

# {{{ __format_c

sub __format_c {
  my $self = shift;
  my ( $operation ) = @_;

  my $argument = $self->advance_argument;
  if ( $operation->{colon} ) {
    return $self->char_name( $argument );
  }
  return $argument;
}

# }}}

# {{{ __format_d

sub __format_d {
  my $self = shift;
  my ( $operation ) = @_;
  $self->_resolve_arguments(
    $operation, [
      [ 'mincol' => 0 ],
      [ 'padchar' => ' ' ],
      [ 'commachar' => ',' ],
      [ 'comma-interval' => 3 ],
    ]
  );

  my $argument = $self->advance_argument;
  $argument = $self->_commify( $argument, $operation );

  if ( $argument and
       $operation->{mincol} > 0 and
       length( $argument ) < $operation->{mincol} ) {
    $argument = ' ' x ( $operation->{mincol} - length( $argument ) ) . $argument;
  }

  return $argument;
}

# }}}

# {{{ __format_f

sub __format_f {
  my $self = shift;
  my ( $operation ) = @_;
  $self->_resolve_arguments(
    $operation, [
      [ w => 0 ],
      [ d => 0 ],
      [ k => 0 ],
      [ overflowchar => ',' ],
      [ padchar => ',' ],
    ]
  );

  my $argument = $self->advance_argument;
  $argument = sprintf "%f", $argument;
  if ( $argument =~ m{ [.] [0]+ $ }x ) {
    $argument =~ s{ [.] [0]+ $ }{.0}x;
  }

  $argument = $self->_commify( $argument, $operation );

  if ( $argument and
       $operation->{w} > 0 and
       length( $argument ) < $operation->{w} ) {
    $argument = ' ' x ( $operation->{w} - length( $argument ) ) . $argument;
  }

  return $argument;
}

# }}}

# {{{ __format_o

sub __format_o {
  my $self = shift;
  my ( $operation ) = @_;
  $self->_resolve_arguments(
    $operation, [
      [ 'mincol' => 0 ],
      [ 'padchar' => ' ' ],
      [ 'commachar' => ',' ],
      [ 'comma-interval' => 3 ],
    ]
  );

  my $argument = $self->advance_argument;
  my $bits = '';
  while ( $argument > 0 ) {
    my $bit = $argument % 8;
    $bits = $bit . $bits;
    $argument -= $bit;
    $argument /= 8;
  }

  $bits = $self->_commify( $bits, $operation );

  if ( $bits and $operation->{mincol} > 0 and length( $bits ) < $operation->{mincol} ) {
    $bits = ' ' x ( $operation->{mincol} - length( $bits ) ) . $bits;
  }

  return $bits;
}

# }}}

# {{{ __format_p

sub __format_p {
  my $self = shift;
  my ( $operation ) = @_;

  if ( $operation->{colon} ) {
    my $argument = $self->previous_argument;
    if ( $operation->{at} ) {
      if ( defined $argument and ( $argument eq 'No' or $argument == 0 ) ) {
        return 'ies';
      }
      elsif ( $argument and $argument == 1 ) {
        return 'y';
      }
      elsif ( $argument and $argument == 2 ) {
        return 'ies';
      }
    }
    if ( defined $argument and ( $argument eq 'No' or $argument == 0 ) ) {
      return 's';
    }
    elsif ( $argument and $argument == 1 ) {
      return '';
    }
    elsif ( $argument and $argument == 2 ) {
      return 's';
    }
  }
  elsif ( $operation->{at} ) {
    my $argument = $self->advance_argument;
    if ( defined $argument and $argument == 0 ) {
      return 'ies';
    }
    elsif ( $argument and $argument == 1 ) {
      return 'y';
    }
    elsif ( $argument and $argument == 2 ) {
      return 'ies';
    }
  }
  else {
    my $argument = $self->advance_argument;
    if ( defined $argument and $argument == 0 ) {
      return 's';
    }
    elsif ( $argument and $argument == 1 ) {
      return '';
    }
    elsif ( $argument and $argument == 2 ) {
      return 's';
    }
  }

  return '';
}

# }}}

# {{{ __format_x

sub __format_x {
  my $self = shift;
  my ( $operation ) = @_;
  $self->_resolve_arguments(
    $operation, [
      [ 'mincol' => 0 ],
      [ 'padchar' => ' ' ],
      [ 'commachar' => ',' ],
      [ 'comma-interval' => 3 ],
    ]
  );

  my $argument = $self->advance_argument;
  my @hexadecimal = qw( 0 1 2 3 4 5 6 7 8 9 a b c d e f );
  my $bits = '';

  while ( $argument > 0 ) {
    my $bit = $argument % 16;
    $bits = $hexadecimal[ $bit ] . $bits;
    $argument -= $bit;
    $argument /= 16;
  }

  $bits = $self->_commify( $bits, $operation );

  if ( $bits and $operation->{mincol} > 0 and length( $bits ) < $operation->{mincol} ) {
    $bits = ' ' x ( $operation->{mincol} - length( $bits ) ) . $bits;
  }

  return $bits;
}

# }}}

# {{{ __format_newline

sub __format_newline {
  my $self = shift;
  my ( $operation ) = @_;
  return "\n" if $operation->{at};
  return "";
}

# }}}

# {{{ __format_percent

sub __format_percent {
  my $self = shift;
  my ( $operation ) = @_;
  $operation->{n} = 1;
  if ( $operation->{arguments} ) {
    my $n = shift @{ $operation->{arguments} };

    $operation->{n} = $n if defined $n;
  }
  delete $operation->{arguments};   

  if ( $operation->{n} and $operation->{n} eq 'v' ) {
    $operation->{n} = shift @{ $self->arguments };
  }
  elsif ( $operation->{n} and $operation->{n} eq '#' ) {
    $operation->{n} = defined $self->arguments ? scalar @{ $self->arguments } : 0;
  }
  $operation->{n} = 1 unless defined $operation->{n};
  return "\n" x $operation->{n};
}

# }}}

# {{{ __format_open_brace

sub __format_open_brace {
  my $self = shift;
  my ( $open, $operation, $close ) = @_;
  my $max_iterations;

  my $output = '';
my $count = 10;
  if ( $self->arguments and ref( $self->arguments ) eq 'ARRAY' and @{ $self->arguments } ) {
    while ( @{ $self->arguments } ) {
      last if defined $max_iterations and $max_iterations-- <= 0;
if ( $count-- < 0 ) {
  return 'OPEN BRACE';
  last;
}
    }
    $output .= $self->_format( $operation );
  }
  return $output;
}

# }}}

# {{{ __format_tilde

sub __format_tilde {
  my $self = shift;
  my ( $operation ) = @_;
  $operation->{n} = 1;
  $self->_resolve_arguments(
    $operation, [ [ n => 0 ] ]
  );

  return "~" x $operation->{n};
}

# }}}

# {{{ __format_vertical_bar

sub __format_vertical_bar {
  my $self = shift;
  my ( $operation ) = @_;
  $self->_resolve_arguments(
    $operation, [ [ n => 1 ] ]
  );

  return "\cL" x $operation->{n};
}

# }}}

# {{{ _format

sub _format {
  my $self = shift;
  my ( $tree ) = @_;
  my $output = '';
  for my $id ( 0 .. $#{ $tree } ) {
    my $operation = $tree->[ $id ];
    if( ref( $operation ) and ref( $operation ) eq 'HASH' ) {
      if ( $operation->{format} eq 'text' ) {
        my $before_newline = 0;
        $before_newline = 1 if
          $id > 0 and
          ref( $tree->[ $id - 1 ] ) and
          ref( $tree->[ $id - 1 ] ) eq 'HASH' and
          $tree->[ $id - 1 ]->{format} eq '~\n';
        my $nl_colon = 0;
        $nl_colon = 1 if
          $before_newline and
          $tree->[ $id - 1 ]->{colon};
        $output .= $self->__format_text( $operation, $before_newline, $nl_colon );
      }
      elsif ( $operation->{format} eq '~a' ) {
        $output .= $self->__format_a( $operation );
      }
      elsif ( $operation->{format} eq '~&' ) {
        my $is_first = $id == 0;
        my $before_percent = 0;
        $before_percent = 1 if
          $id > 0 and
          ref( $tree->[ $id - 1 ] ) and
          ref( $tree->[ $id - 1 ] ) eq 'HASH' and
          $tree->[ $id - 1 ]->{format} eq '~%';
        $output .= $self->__format_ampersand( $operation, $is_first, $before_percent );
      }
      elsif ( $operation->{format} eq '~b' ) {
        $output .= $self->__format_b( $operation );
      }
      elsif ( $operation->{format} eq '~c' ) {
        $output .= $self->__format_c( $operation );
      }
      elsif ( $operation->{format} eq '~d' ) {
        $output .= $self->__format_d( $operation );
      }
      elsif ( $operation->{format} eq '~f' ) {
        $output .= $self->__format_f( $operation );
      }
      elsif ( $operation->{format} eq '~\n' ) {
        $output .= $self->__format_newline( $operation );
      }
      elsif ( $operation->{format} eq '~o' ) {
        $output .= $self->__format_o( $operation );
      }
      elsif ( $operation->{format} eq '~p' ) {
        $output .= $self->__format_p( $operation );
      }
      elsif ( $operation->{format} eq '~%' ) {
        $output .= $self->__format_percent( $operation );
      }
      elsif ( $operation->{format} eq '~~' ) {
        $output .= $self->__format_tilde( $operation );
      }
      elsif ( $operation->{format} eq '~x' ) {
        $output .= $self->__format_x( $operation );
      }
      elsif ( $operation->{format} eq '~|' ) {
        $output .= $self->__format_vertical_bar( $operation );
      }
      else {
        $output = 'UNIMPLEMENTED FORMAT'; last;
      }
    }
    elsif ( ref( $operation ) and ref( $operation ) eq 'ARRAY' ) {
      my ( $open, $_operation, $close ) = @{ $operation };
      my $_arguments;
      if ( $self->arguments and ref( $self->arguments ) eq 'ARRAY' ) {
        $_arguments = $self->arguments->[0];
      }
      if ( $open->{format} eq '~{' ) {
        $output .= $self->__format_open_brace(
          $open, $_operation, $close, $_arguments
        );
      }
      elsif ( $open->{format} eq '~(' ) {
        $output .= $self->__format_open_paren(
          $open, $_operation, $close, $_arguments
        );
      }
      elsif ( $open->{format} eq '~[' ) {
        $output .= $self->__format_open_bracket(
          $open, $_operation, $close, $_arguments
        );
      }
    }
    else {
      croak "Unknown operation type";
    }

  }
  return $output;
}

# }}}

# {{{ apply

sub apply {
  my $self = shift;

  if ( my $tree = $self->parser->from_string( $self->format ) ) {
    return $self->_format( $tree );
  }
  
  return 'Not Caught';
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
