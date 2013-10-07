package JGoff::Lisp::Format::Tokens::Asterisk;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has parameters => ( is => 'rw' );
has n => ( is => 'rw' );
has at => ( is => 'ro', isa => 'Bool' );
has colon => ( is => 'ro', isa => 'Bool' );

=head1 NAME

JGoff::Lisp::Format::Tokens::Asterisk - Internal token for parser

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
  $self->_resolve_parameters(
    $core, [
      [ 'n' => 1 ],
    ]
  );
  $self->n( 1 ) unless defined $self->n;

  if ( $self->colon ) {
    for ( 1 .. $self->n ) {
      $core->backward_argument;
    }
  }
  elsif ( $self->at ) {
    $core->backward_argument;
  }
  else {
    if ( defined $self->n and
         $self->n == 1 ) {
      $core->forward_argument;
    }
  }

  return "";
}

1;
