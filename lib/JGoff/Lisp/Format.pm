package JGoff::Lisp::Format;

use Moose;

use Readonly;

use JGoff::Lisp::Format::Core;

Readonly our $upcase => 'upcase';
Readonly our $downcase => 'downcase';
Readonly our $capitalize => 'capitalize';

our $print_case = $upcase; # default value from the CLISP spec
our $print_radix = undef; # XXX ?

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

# {{{ format

sub format {
  my $self = shift;
  my ( $stream, $format, $arguments ) = @_;

  my $f = JGoff::Lisp::Format::Core->new(
    stream => $stream,
    format => $format,
    arguments => $arguments,

    print_case => $print_case,
    print_radix => $print_radix,
  );

  return $f->apply;
}

# }}}

=head2 formatter

=cut

# {{{ formatter

sub formatter {
  my $self = shift;
  my ( $format ) = @_;

  return sub {
    my ( $stream, $arguments ) = @_;
    my $_format = JGoff::Lisp::Format::Core->new(
      stream => $stream,
      format => $format,
      arguments => $arguments,

      print_case => $print_case,
      print_radix => $print_radix,
    );
    return $_format->apply;
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
