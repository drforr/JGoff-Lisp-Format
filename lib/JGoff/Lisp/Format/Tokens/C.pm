package JGoff::Lisp::Format::Tokens::C;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has colon => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core ) = @_;

  my $argument = $core->current_argument;
  $core->forward_argument;
  if ( $self->colon ) {
    return $self->char_name( $argument );
  }
  return $argument;
}

# }}}
1;
