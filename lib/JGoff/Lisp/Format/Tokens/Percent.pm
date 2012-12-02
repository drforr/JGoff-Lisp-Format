package JGoff::Lisp::Format::Tokens::Percent;

use Moose;

has n => ( is => 'rw' );
has at => ( is => 'ro' );
has colon => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core ) = @_;
  $self->n( 1 ) unless defined $self->n;

  if ( $self->n and $self->n eq 'v' ) {
    $self->n( $core->current_argument );
    $core->forward_argument;
  }
  elsif ( $self->n and $self->n eq '#' ) {
    $self->n( $core->num_arguments );
  }
  $self->n( 1 ) unless defined $self->n;
  return "\n" x $self->n;
}

1;
