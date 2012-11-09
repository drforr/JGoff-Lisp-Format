package JGoff::Lisp::Format::Parser;

use Moose;

extends 'Parser::MGC';

=head1 NAME

JGoff::Lisp::Format::Parser - Parser internals

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

Quick summary of what the module does.

    use JGoff::Lisp::Format::Parser;

    my $foo = JGoff::Lisp::Format->new();
    ...

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.

=head1 METHODS

=head2 parse( $text )

=cut

sub __token_a {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: : |
          @ |
          v |
          v: |
          v@ |
          v:@ | # :@ and @: are equivalent
          v[@]: |
          \d+,\d+ |
          \d+,\d+@ |
      )?
      [aA] 
  }x );
  my $rv = { format => '~a' };
  $rv->{colon} = 1 if $match =~ m{ [:] | [:][@] [aA] }x;
  $rv->{at} = 1 if $match =~ m{ [@] | [@][:] [aA] }x;
  $rv->{v} = 1 if $match =~ m{ [v] | [v][:] | [v][@] [aA] }x;
  if ( $match =~ m{ (\d+),(\d+) [@]? [aA] }x ) {
    $rv->{minpad} = $1;
    $rv->{colinc} = $2;
  }
  return $rv;
}

sub parse {
  my $self = shift;

  $self->sequence_of( sub {
    $self->any_of(
      sub { $self->__token_a },
    );
  } );
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
