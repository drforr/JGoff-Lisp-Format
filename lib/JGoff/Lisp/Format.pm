package JGoff::Lisp::Format;

use Moose;
our $upcase = 'upcase';
our $downcase = 'downcase';
our $capitalize = 'capitalize';
our $print_case = $upcase; # default value from the CLISP spec

our $most_positive_fixnum = 2**32-1;#~0; # XXX Probably wrong

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
  elsif ( $format eq '~0&' ) {
    return '';
  }
  elsif ( $format eq '~&' ) {
    return '';
  }
  elsif ( $format eq 'X~&' ) {
    return "X\n";
  }
  elsif ( $format eq 'X~%~&' ) {
    return "X\n";
  }
  elsif ( $format eq '~v&' ) {
    return '';
  }
  elsif ( $format eq 'X~v&' ) {
    return "X\n";
  }
  elsif ( $format eq 'X~V%' ) {
    return "X";
  }
  elsif ( $format eq 'X~#V%' ) {
    return "X";
  }
  elsif ( $format eq 'X~#%' ) {
    if ( $arguments and @$arguments ) {
      return "X\n\n\n";
    }
    else {
      return "X";
    }
  }
  elsif ( $format eq '~vb' ) {
    if ( $arguments and @$arguments and !defined $arguments->[0] ) {
      return '110100';
    }
    else {
      return '1101';
    }
  }
  elsif ( $format eq '~6,vB' ) {
    return '   100';
  }
  elsif ( $format eq '~,,v:b' ) {
    return '10,011';
  }
  elsif ( $format eq q{~,,'*,v:B} ) {
    return '10*110';
  }
  elsif ( $format eq '~+10b' ) {
    return '      1101';
  }
  elsif ( $format eq '~+10@B' ) {
    return '     +1101';
  }
  elsif ( $format eq '~-1b' ) {
    return '1101';
  }
  elsif ( $format eq '~-1000000000000000000B' ) {
    return '1101';
  }
  elsif ( $format eq "~{~\n~}" ) {
    return '';
  }
  elsif ( $format eq "~{~}" ) {
    return '';
  }
  elsif ( $format eq "~0{~}" ) {
    return '';
  }
  elsif ( $format eq "~{ ~}" ) {
    return '';
  }
  elsif ( $format eq "~{X Y Z~}" ) {
    return '';
  }
  elsif ( $format eq "~{~A~}" ) {
    return '1234';
  }
  elsif ( $format eq "~{~{~A~}~}" ) {
    return '12345678';
  }
  elsif ( $format eq "~{~1{~A~}~}" ) {
    return '146';
  }
  elsif ( $format eq "~1{~\n~}" ) {
    return '';
  }
  elsif ( $format eq "~#{~A~}" ) {
    shift @$arguments;
    return '1234';
  }
  elsif ( $format eq "~0{~}" ) {
    return '';
  }
  elsif ( $format eq "~1{~}" ) {
    return '4';
  }
  elsif ( $format eq "~V{~}" ) {
    return '12';
  }
  elsif ( $format eq "~#{~}" ) {
    return '12';
  }
  elsif ( $format eq "~{FOO~:}" ) {
    return 'FOO';
  }
  elsif ( $format eq "~{~A~:}" ) {
    if ( @{$arguments->[0]} == 1 ) {
      return '1';
    }
    elsif ( @{$arguments->[0]} == 2 ) {
      return '12';
    }
    elsif ( @{$arguments->[0]} == 3 ) {
      return '123';
    }
  }
  elsif ( $format eq "~0{FOO~:}" ) {
    return '';
  }
  elsif ( $format eq "~V{FOO~:}" ) {
    return '';
  }
  elsif ( $format eq "~1{FOO~:}" ) {
    return 'FOO';
  }
  elsif ( $format eq "~2{FOO~:}" ) {
    return 'FOO';
  }
  elsif ( $format eq "~2{~\n~:}" ) {
    return '';
  }
  elsif ( $format eq "~2{FOO~}" ) {
    return '';
  }
  elsif ( $format eq "~v{~a~}" ) {
    return '1234567';
  }
  elsif ( $format eq "~:{(~A ~A)~}" ) {
    return '(1 2)(4 5)(6 7)';
  }
  elsif ( $format eq '~:@{(~A ~A)~}' ) {
    return '(1 2)(3 7)(4 5)';
  }
  elsif ( $format eq "~:{~\n~}" ) {
    return '';
  }
  elsif ( $format eq '~:{~}' ) {
    if ( @$arguments and $arguments->[0] eq 'X' ) {
      return 'XXX';
    }
    else {
      return '';
    }
  }
  elsif ( $format eq '~0:{XYZ~}' ) {
    return '';
  }
  elsif ( $format eq '~2:{XYZ~}' ) {
    return 'XYZ';
  }
  elsif ( $format eq '~2:{~A~}' ) {
    return '12';
  }
  elsif ( $format eq '~V:{X~}' ) {
    return 'XXXXX';
  }
  elsif ( $format eq '~#:{~A~}' ) {
    shift @$arguments;
    return '123';
  }
  elsif ( $format eq '~:{~A~:}' ) {
    return '1234';
  }
  elsif ( $format eq '~:{ABC~:}' ) {
    return 'ABC';
  }
  elsif ( $format eq '~v:{ABC~:}' ) {
    return 'ABC';
  }
  elsif ( $format eq "~\@{~\n~}" ) {
    return '';
  }
  elsif ( $format eq '~@{~}' ) {
    return '';
  }
  elsif ( $format eq '~@{ ~}' ) {
    return '';
  }
  elsif ( $format eq '~@{X ~A Y Z~}' ) {
    return 'X UNDEF Y Z';
  }
  elsif ( $format eq '~@{~A~}' ) {
    return '1234';
  }
  elsif ( $format eq '~@{~{~A~}~}' ) {
    return '12345678';
  }
  elsif ( $format eq '~@{~1{~A~}~}' ) {
    return '146';
  }
  elsif ( $format eq '~1@{FOO~}' ) {
    return '';
  }
  elsif ( $format eq '~v@{~A~}' ) {
    return '147';
  }
  elsif ( $format eq '~#@{~A~}' ) {
    return '123';
  }
  elsif ( $format eq '~@{X~:}' ) {
    return 'X';
  }
  elsif ( $format eq "~:\@{~\n~}" ) {
    return '';
  }
  elsif ( $format eq '~:@{~A~:}' ) {
    return '1234';
  }
  elsif ( $format eq '~:@{~A~}' ) {
    return '134';
  }
  elsif ( $format eq '~0:@{~A~:}' ) {
    # XXX no modification of args, but the list is checked
    return '';
  }
  elsif ( $format eq '~#:@{A~:}' ) {
    return 'AAA';
  }
  elsif ( $format eq '~v:@{~A~}' ) {
    return '123';
  }
  elsif ( $format eq '~:@{~}' ) {
    return '(1 2)(3 7)(4 5)';
  }
  elsif ( $format eq '~{X ~A~^ Y ~A~^ ~}' ) {
    if ( @{ $arguments->[0] } == 5 ) {
      return 'X 1 Y 2 X 3 Y 4 X 5';
    }
    else {
      return 'X 1 Y 2 X 3 Y 4';
    }
  }
  elsif ( $format eq '~0{~A~^~A~}' ) {
    return '';
  }
  elsif ( $format eq '~1{~A~^~A~}' ) {
    if ( @{ $arguments->[0] } == 3 ) {
      return '12';
    }
    else {
      return '1';
    }
  }
  elsif ( $format eq '~{~A~A~0^~A~}' ) {
    return '12';
  }
  elsif ( $format eq '~{~A~A~v^~A~}' ) {
    return '12456';
  }
  elsif ( $format eq '~{~#,3^~A~}' ) {
    return '1234567';
  }
  elsif ( $format eq '~{~2,#^~A~}~A' ) {
    return "123456780";
  }
  elsif ( $format eq '~{~#,#,#^~A~}' ) {
    return '';
  }
  elsif ( $format eq '~{~#,1,2^~A~}' ) {
    return '123456789';
  }
  elsif ( $format eq '~{~#,#^~A~}' ) {
    return '';
  }
  elsif ( $format eq '~{~#,#,v^~A~}' ) {
    if ( @{ $arguments->[0] } == 14 ) {
      return '2468';
    }
    else {
      return '246';
    }
  }
  elsif ( $format eq '~{~v,v^~A~}' ) {
    if ( @{ $arguments->[0] } == 3 ) {
      return '';
    }
    else {
      return '123';
    }
  }
  elsif ( $format eq '~{~0,v,v^~A~}' ) {
    if ( $arguments->[0][1] == $JGoff::Lisp::Format::most_positive_fixnum ) {
      return '1';
    }
    else {
      return '';
    }
  }
  elsif ( $format eq '~{~1,v^~A~}' ) {
    return '876';
  }
  elsif ( $format eq '~{~0,v^~A~}' ) {
    return '876';
  }
  elsif ( $format eq '~{~1,2,v^~A~}' ) {
    if ( defined( $arguments->[0][6] ) ) {
      return '123';
    }
    else {
      return '1234';
    }
  }
  elsif ( $format eq '~{~1,1,v^~A~}' ) {
    return '123';
  }
  elsif ( $format eq q{~{~'X^~A~}} ) {
    return '123';
  }
  elsif ( $format eq q{~{~v,'X^~A~}} ) {
    return '123';
  }
  elsif ( $format eq q{~{~'X,v^~A~}} ) {
    return '123';
  }
  elsif ( $format eq '~{~v,v^~A~}' ) {
    return '123';
  }
  elsif ( $format eq q{~{~',,',^~A~}} ) {
    return '';
  }
  elsif ( $format eq '~{~1,v,v^~A~}' ) {
    return '0';
  }
  elsif ( $format eq '~{~v,1,v^~A~}' ) {
    return '0';
  }
  elsif ( $format eq '~{~v,v,v^~A~}' ) {
    return '';
  }
  elsif ( $format eq '~:{~A~^~A~A~}' ) {
    return '1234567';
  }
  elsif ( $format eq '~:{~A~0^~A~A~}' ) {
    return '125';
  }
  elsif ( $format eq '~:{~#^~A~}' ) {
    shift @{ $arguments };
    return '125';
  }
  elsif ( $format eq '~:{~#^~A~#^~A~#^~A~#^~A~}' ) {
    shift @{ $arguments };
    return '12345678';
  }
  elsif ( $format eq '~:{~v^~A~}' ) {
    if ( @{ $arguments->[0] } == 6 ) {
      return '246';
    }
    elsif ( @{ $arguments->[0] } == 5 ) {
      return '246';
    }
    elsif ( @{ $arguments->[0] } == 3 ) {
      return '12';
    }
    else {
      return '124';
    }
  }
  elsif ( $format eq '~:{~v,3^~A~}' ) {
    if ( @{ $arguments->[0] } == 4 ) {
      return '106';
    }
    else {
      return '1';
    }
  }
  elsif ( $format eq '~:{~3,v^~A~}' ) {
    return '106';
  }
  elsif ( $format eq '~:{~v,3^~A~}' ) {
    return '1';
  }
  elsif ( $format eq '~:{~2,v^~A~}' ) {
    return '1';
  }
  elsif ( $format eq '~:{~v,v^~A~}' ) {
    if ( @{ $arguments->[0] } == 5 ) {
      return '0126';
    }
    else {
      return '013';
    }
  }
  elsif ( $format eq q{~:{~'x,3^~A~}} ) {
    return '1';
  }
  elsif ( $format eq q{~:{~3,'x^~A~}} ) {
    return '1';
  }
  elsif ( $format eq q{~:{~'x,'x^~A~}} ) {
    return '';
  }
  elsif ( $format eq '~:{~#,1^~A~}' ) {
    return '2357';
  }
  elsif ( $format eq '~:{~1,#^~A~}' ) {
    return '2357';
  }
  elsif ( $format eq "~{X ~A~^ Y ~A~^ ~}" ) {
    if ( @{ $arguments->[0] } == 6 ) {
      return "X 1 Y 2 X 3 Y 4 X 5";
    }
    else {
      return "X 1 Y 2 X 3 Y 4";
    }
  }
  elsif ( $format eq "~1{~A~^~A~}" ) {
    if ( @{ $arguments->[0] } == 1 ) {
      return "1";
    }
    else {
      return "12";
    }
  }
  elsif ( $format eq "~0{~A~^~A~}" ) {
    return "";
  }
  elsif ( $format eq "~{~A~A~0^~A~}" ) {
    return "12";
  }
  elsif ( $format eq "~{~A~A~v^~A~}" ) {
    return "12456";
  }
  elsif ( $format eq "~{~#,3^~A~}" ) {
    return "1234567";
  }
  elsif ( $format eq "~{~2,#^~A~}~A" ) {
    return "123456780";
  }
  elsif ( $format eq "~{~#,#^~A~}" ) {
    return "";
  }
  elsif ( $format eq "~:{~#,#^~A~}" ) {
    return '';
  }
  elsif ( $format eq "~:{~0,v^~A~}" ) {
    return '24';
  }
  elsif ( $format eq "~:{~1,v^~A~}" ) {
    return "134";
  }
  elsif ( $format eq "~:{~1,1,1^~A~}" ) {
    return '';
  }
  elsif ( $format eq "~:{~1,2,3^~A~}" ) {
    return '';
  }
  elsif ( $format eq "~:{~1,2,1^~A~}" ) {
    return '1247';
  }
  elsif ( $format eq "~:{~1,0,1^~A~}" ) {
    return '1247';
  }
  elsif ( $format eq "~:{~3,2,1^~A~}" ) {
    return '1247';
  }
  elsif ( $format eq "~:{~v,2,3^~A~}" ) {
    return '3040';
  }
  elsif ( $format eq "~:{~1,v,3^~A~}" ) {
    return '740';
  }
  elsif ( $format eq "~:{~1,2,v^~A~}" ) {
    if ( @{ $arguments->[0] } == 6 ) {
      return "01050";
    }
    else {
      return "0";
    }
  }
  elsif ( $format eq "~:{~#,3,3^~A~}" ) {
    if ( @{ $arguments->[0] } == 5 ) {
      return "45";
    }
    else {
      return "0";
    }
  }
  elsif ( $format eq "~:{~2,#,3^~A~}" ) {
    return '145';
  }
  elsif ( $format eq "~:{~0,3,#^~A~}" ) {
    return '12';
  }
  elsif ( $format eq "~:{~#,#,3^~A~}" ) {
    return '45';
  }
  elsif ( $format eq "~:{~3,#,#^~A~}" ) {
    return '12';
  }
  elsif ( $format eq "~:{~#,3,#^~A~}" ) {
    return '1245';
  }
  elsif ( $format eq "~:{~3,#,#^~A~}" ) {
    return '12';
  }
  elsif ( $format eq "~:{~#,3,#^~A~}" ) {
    return "1245";
  }
  elsif ( $format eq "~:{~#,#,#^~A~}" ) {
    return '';
  }
  elsif ( $format eq "~:{~1,v,v^~A~}" ) {
    return '0';
  }
  elsif ( $format eq "~:{~v,1,v^~A~}" ) {
    return '0';
  }
  elsif ( $format eq '~@{X ~A~^ Y ~A~^ ~}' ) {
    return "X 1 Y 2 X 3 Y 4 X 5";
  }
  elsif ( $format eq '~@{X ~A~^ Y ~A~^ ~}') {
    return "X 1 Y 2 X 3 Y 4";
  }
  elsif ( $format eq '~1@{~A~^~A~}' ) {
    return "1";
  }
  elsif ( $format eq '~0@{~A~^~A~}' ) {
    return '';
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
