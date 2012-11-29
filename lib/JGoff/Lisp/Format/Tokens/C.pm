package JGoff::Lisp::Format::Tokens::C;

use Moose;

has colon => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core ) = @_;

  my $argument = $core->increment_argument;
  if ( $self->colon ) {
    return $core->char_name( $argument );
  }
  return $argument;
}

# }}}
1;
