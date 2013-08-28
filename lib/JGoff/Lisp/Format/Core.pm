package JGoff::Lisp::Format::Core;

use Moose;
use Scalar::Util qw( blessed ); # XXX temporary

use Readonly;

use JGoff::Lisp::Format::Parser;
use JGoff::Lisp::Format::Token; # XXX rethink this
use Carp qw( croak );

has argument_index => ( is => 'rw', isa => 'Int', default => 0 );
has arguments => ( is => 'rw' );

=head1 NAME

JGoff::Lisp::Format::Core - Core parser class

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

Core module, only use JGoff::Lisp::Format

=head1 METHODS

=head2 current_argument()

=cut

sub current_argument {
  my $self = shift;
  return undef unless $self->arguments and
                      ref( $self->arguments ) and
                      ref( $self->arguments ) eq 'ARRAY' and
                      @{ $self->arguments };
  return $self->arguments->[ $self->argument_index ];
}

=head2 num_arguments()

=cut

sub num_arguments {
  my $self = shift;
  return 0 unless $self->arguments and
                  ref( $self->arguments ) and
                  ref( $self->arguments ) eq 'ARRAY' and
                  @{ $self->arguments };
  return scalar @{ $self->arguments };
}

=head2 remaining_arguments()

=cut

sub remaining_arguments {
  my $self = shift;
  return undef unless $self->arguments and
                      ref( $self->arguments ) and
                      ref( $self->arguments ) eq 'ARRAY' and
                      @{ $self->arguments };
  return $self->arguments->[ $self->argument_index .. $#{ $self->arguments } ];
  return scalar @{ $self->arguments };
}

=head2 forward_argument()

=cut

sub forward_argument {
  my $self = shift;
  return unless $self->arguments and @{ $self->arguments };
  if ( $self->argument_index < $#{ $self->arguments } ) {
    $self->argument_index( $self->argument_index + 1 );
  }
}

=head2 backward_argument()

=cut

sub backward_argument {
  my $self = shift;
  return unless $self->arguments and @{ $self->arguments };
  if ( $self->argument_index > 0 ) {
    $self->argument_index( $self->argument_index - 1 );
  }
}

has stream => ( is => 'rw' );
has format => ( is => 'rw' );
has tree => ( is => 'rw' );
has parser => (
  is => 'rw',
  isa => 'JGoff::Lisp::Format::Parser',
  default => sub { JGoff::Lisp::Format::Parser->new }
);

Readonly our $most_positive_fixnum => 2**32-1;#~0; # XXX Probably wrong
Readonly our $most_negative_fixnum => -(2**32-1);#~0; # XXX Probably wrong

has print_case => ( is => 'ro', default => $JGoff::Lisp::Format::Token::upcase );

# {{{ _format

sub _format {
  my $self = shift;
  my %token_names = map { 'JGoff::Lisp::Format::Tokens::' . $_ => 1 } qw(
    A Asterisk
    B
    C Circumflex
    D
    F
    Newline
    O
    P Percent
    Question
    R
    S
    Tilde
    Vertical_Bar
    X
  );
  my $tree = $self->tree;
  my $output = '';
  for my $id ( 0 .. $#{ $tree } ) {
    my $operation = $tree->[ $id ];
    if ( blessed( $operation ) ) {
      if ( $operation->isa( 'JGoff::Lisp::Format::Tokens::Circumflex' ) ) {
        if ( $self->argument_index >= $self->num_arguments ) {
          last;
        }
      }
      if ( $operation->isa( 'JGoff::Lisp::Format::Tokens::Text' ) ) {
        my $before_newline = 0;
        $before_newline = 1 if
          $id > 0 and
          $tree->[ $id - 1 ]->isa( 'JGoff::Lisp::Format::Tokens::Newline' );
        my $nl_colon = 0;
        $nl_colon = 1 if
          $before_newline and
          $tree->[ $id - 1 ]->colon;
        $output .= $operation->format( $before_newline, $nl_colon );
      }
      elsif ( $operation->isa( 'JGoff::Lisp::Format::Tokens::Ampersand' ) ) {
        my $is_first = $id == 0;
        my $before_percent = 0;
        $before_percent = 1 if
          $id > 0 and
          $tree->[ $id - 1 ]->isa( 'JGoff::Lisp::Format::Tokens::Percent' );
        $output .= $operation->format( $is_first, $before_percent );
      }
      elsif ( exists $token_names{ ref( $operation ) } ) {
        $output .= $operation->format( $self );
      }
    }
    elsif ( ref( $operation ) and ref( $operation ) eq 'ARRAY' ) {
      my ( $open, $_operation, $close ) = @{ $operation };
      if ( $open->isa( 'JGoff::Lisp::Format::Tokens::Open_Brace' ) ) {
        $output .= $open->format( $self, $_operation, $close );
      }
      else {
        croak "Unknown sub-operation type";
      }
    }
    else {
      croak "Unknown operation type";
    }
  }
  return $output;
}

# }}}

=head2 apply()

=cut

# {{{ apply

sub apply {
  my $self = shift;

  if ( my $tree = $self->parser->from_string( $self->format ) ) {
    $self->tree( $tree );
    return $self->_format;
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
