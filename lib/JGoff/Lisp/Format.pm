package JGoff::Lisp::Format;

use Moose;

use Readonly;

use JGoff::Lisp::Format::Parser;
use Carp qw( croak );

Readonly our $upcase => 'upcase';
Readonly our $downcase => 'downcase';
Readonly our $capitalize => 'capitalize';

our $print_case = $upcase; # default value from the CLISP spec

Readonly our $most_positive_fixnum => 2**32-1;#~0; # XXX Probably wrong
Readonly our $most_negative_fixnum => -(2**32-1);#~0; # XXX Probably wrong

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

# An arg, any object, is printed without escape characters (as by princ). If arg is a string, its characters will be output verbatim. If arg is nil it will be printed as nil; the colon modifier (~:A) will cause an arg of nil to be printed as (), but if arg is a composite structure, such as a list or vector, any contained occurrences of nil will still be printed as nil.

#~mincolA inserts spaces on the right, if necessary, to make the width at least mincol columns. The @ modifier causes the spaces to be inserted on the left rather than the right.

#~mincol,colinc,minpad,padcharA is the full form of ~A, which allows control of the padding. The string is padded on the right (or on the left if the @ modifier is used) with at least minpad copies of padchar; padding characters are then inserted colinc characters at a time until the total width is at least mincol. The defaults are 0 for mincol and minpad, 1 for colinc, and the space character for padchar. 

# ~mincol,colinc,minpad,padcharA
#
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

  my $argument = shift @{ $arguments };

# Strip escape characters

if ( !defined $argument ) {
  if ( $element->{colon} ) {
    return '[]';
  }
  $argument = $self->_padding( $element, 'undef' );
  return $self->_print_case( $argument );
}
elsif ( ref( $argument ) and ref( $argument ) eq 'ARRAY' ) {
 return '[' . $self->format( undef, $element->{format}, @{ $argument } ) . ']';
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

sub format {
  my $self = shift;
  my ( $stream, $format, $arguments ) = @_;

  my $parser = JGoff::Lisp::Format::Parser->new( patterns => { ws => undef } );
  if ( my $tree = $parser->from_string( $format ) ) {
    my $output;
    for my $element ( @{ $tree } ) {
if( ref( $element ) ) {
      if ( $element->{format} eq '~a' ) {
        $output .= $self->__format_a( $element, $arguments );
      }
      else {
        $output = 'UNIMPLEMENTED FORMAT'; last;
      }
}
else {
  $output .= $element;
}

    }
    return $output;
  }
  
  return 'Not Caught';
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
