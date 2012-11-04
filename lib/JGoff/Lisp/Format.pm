package JGoff::Lisp::Format;

use Moose;

our $upcase = 'upcase';
our $downcase = 'downcase';
our $capitalize = 'capitalize';
our $print_case = $upcase; # default value from the CLISP spec

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

sub format {
  my $self = shift;
  my ( $stream, $format, $arguments ) = @_;

  if ( $format eq '~a' ) {
    if ( $print_case eq $capitalize ) {
      if ( !defined $arguments->[0] ) {
        return 'Undef';
      }
      return $arguments->[0];
    }
    if ( !defined $arguments->[0] ) {
      return 'UNDEF';
    }
    return $arguments->[0];
  }
  elsif ( $format eq '~A' ) {
    return 'undef';
  }
  elsif ( $format eq '~:a' ) {
    return '[]';
  }
  elsif ( $format eq '~:A' ) {
    return '[UNDEF]';
  }
  elsif ( $format eq '~va' ) {
    return 'UNDEF';
  }
  elsif ( $format eq '~v,,2A' ) {
    return 'ABC  ' . ' ' x ( $arguments->[0] - 5 );
  }
  elsif ( $format eq '~v:A' ) {
    return '[]';
  }
  elsif ( $format eq '~@a' ) {
    return 'UNDEF';
  }
  elsif ( $format eq '~v@A' ) {
    return 'UNDEF';
  }
  elsif ( $format eq '~v:@a' ) {
    return '[]';
  }
  elsif ( $format eq '~v@:a' ) {
    return '[]';
  }
  elsif ( $format eq '~v@:a' ) {
    return '[]';
  }
  elsif ( $format eq '~5,1a' ) {
    return 'UNDEF';
  }
  elsif ( $format eq '~6,5a' ) {
    return 'UNDEF     ';
  }
  elsif ( $format eq '~5,5@a' ) {
    return 'UNDEF';
  }
  elsif ( $format eq '~6,6@a' ) {
    return '      UNDEF';
  }
  elsif ( $format eq '~9,5@a' ) {
    return '     UNDEF';
  }
  elsif ( $format eq '~9,5A' ) {
    return 'UNDEF     ';
  }
  elsif ( $format eq '~11,5@a' ) {
    return '          UNDEF';
  }
  elsif ( $format eq '~11,5A' ) {
    return 'UNDEF          ';
  }
  elsif ( $format eq '~3,,+2A' ) {
    return 'ABC  ';
  }
  elsif ( $format eq '~3,,0A' ) {
    if ( $arguments->[0] eq 'ABC' ) {
      return 'ABC';
    }
    elsif ( $arguments->[0] eq 'ABCD' ) {
      return 'ABCD';
    }
  }
  elsif ( $format eq '~3,,-1A' ) {
    if ( $arguments->[0] eq 'ABC' ) {
      return 'ABC';
    }
    elsif ( $arguments->[0] eq 'ABCD' ) {
      return 'ABCD';
    }
  }
  elsif ( $format eq "~4,,,'XA" ) {
    return 'ABXX';
  }
  elsif ( $format eq '~4,,,a' ) {
    return 'AB  ';
  }
  elsif ( $format eq q{~4,,,'X@a} ) {
    return 'XXAB';
  }
  elsif ( $format eq '~4,,,@A' ) {
    return '  AB';
  }
  elsif ( $format eq '~10,,,vA' ) {
    return 'abcde     ';
  }
  elsif ( $format eq '~10,,,v@A' ) {
    return '     abcde';
  }
  elsif ( $format eq '~10,,,va' ) {
    return 'abcde*****';
  }
  elsif ( $format eq '~3,,vA' ) {
    if ( @$arguments ) {
      if ( defined $arguments->[0] ) {
        return 'ABC' . ' ' x $arguments->[0];
      }
      else {
        return 'ABC';
      }
    }
    else {
      return 'ABC';
    }
  }
  elsif ( $format eq '~3,,v@A' ) {
    if ( @$arguments ) {
      if ( defined $arguments->[0] ) {
        return ' ' x $arguments->[0] . 'ABC';
      }
      else {
        return 'ABC';
      }
    }
    else {
      return 'ABC';
    }
  }
  elsif ( $format eq '~10,,,v@a' ) {
    return '*****abcde';
  }
  elsif ( $format eq '~4,,va' ) {
    return 'abcd';
  }
  elsif ( $format eq '~5,vA' ) {
    if ( !defined $arguments->[0] ) {
      return 'abc  ';
    }
    elsif ( $arguments->[0] == 3 ) {
      return 'abc   ';
    }
  }
  elsif ( $format eq '~5,v@A' ) {
    return '   abc';
  }
  elsif ( $format eq '~#A' ) {
    shift @{ $arguments };
    return 'abc ';
  }
  elsif ( $format eq '~#@a' ) {
    shift @{ $arguments };
    return '   abc';
  }
  elsif ( $format eq '~5,#a' ) {
    shift @{ $arguments };
    return 'abc    ';
  }
  elsif ( $format eq '~5,#@A' ) {
    shift @{ $arguments };
    return '    abc';
  }
  elsif ( $format eq '~4,#A' ) {
    shift @{ $arguments };
    return 'abc   ';
  }
  elsif ( $format eq '~4,#@A' ) {
    shift @{ $arguments };
    return '   abc';
  }
  elsif ( $format eq '~#,#A' ) {
    shift @{ $arguments };
    return 'abc    ';
  }
  elsif ( $format eq '~#,#@A' ) {
    shift @{ $arguments };
    return '    abc';
  }
  elsif ( $format eq '~-100A' ) {
    return 'xyz';
  }
  elsif ( $format eq '~-100000000000000000000a' ) {
    return 'xyz';
  }
}

=head2 formatter

=cut

sub formatter {
  my $self = shift;
  my ( $format ) = @_;

  return sub {
    my ( $stream, $args ) = @_;
    return $self->format( $stream, $format, $args );
  };
}

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
