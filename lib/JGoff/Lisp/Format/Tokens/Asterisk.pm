package JGoff::Lisp::Format::Tokens::Asterisk;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has arguments => ( is => 'rw' );
has n => ( is => 'rw' );
has at => ( is => 'ro' );
has colon => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core ) = @_;
  $self->_resolve_arguments(
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
    if ( defined $self->n and $self->n == 1 ) {
      $core->forward_argument;
    }
  }

  return "";
}

1;
