package JGoff::Lisp::Format::Tokens::Percent;

use Moose;

has n => ( is => 'rw' );
has at => ( is => 'ro', isa => 'Bool' );
has colon => ( is => 'ro', isa => 'Bool' );

=head1 NAME

JGoff::Lisp::Format::Tokens::Percent - Internal token for parser

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
  $self->n( 1 ) unless defined $self->n;

  if ( $self->n and
       $self->n eq 'v' ) {
    $self->n( $core->current_argument );
    $core->forward_argument;
  }
  elsif ( $self->n and
          $self->n eq '#' ) {
    $self->n( $core->num_arguments );
  }
  $self->n( 1 ) unless defined $self->n;
  return "\n" x $self->n;
}

1;
