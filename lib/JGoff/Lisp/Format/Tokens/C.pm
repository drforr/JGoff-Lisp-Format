package JGoff::Lisp::Format::Tokens::C;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has colon => ( is => 'ro', isa => 'Bool' );

=head1 NAME

JGoff::Lisp::Format::Tokens::C - Internal token for parser

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 METHODS

=head2 format( $core )

=cut

sub format {
  my $self = shift;
  my ( $core ) = @_;

  my $argument = $core->forward_argument;
  if ( $self->colon ) {
    return $self->char_name( $argument );
  }
  return $argument;
}

1;
