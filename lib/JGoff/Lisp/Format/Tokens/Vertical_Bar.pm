package JGoff::Lisp::Format::Tokens::Vertical_Bar;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has arguments => ( is => 'rw' );
has colon => ( is => 'ro' );
has at => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core ) = @_;
  $self->_resolve_arguments(
    $core, [
      [ n => 1 ] 
    ]
  );

  return "\cL" x $self->{n};
}

# }}}
1;
