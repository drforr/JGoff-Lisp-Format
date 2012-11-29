package JGoff::Lisp::Format::Tokens::Tilde;

use Moose;

has n => ( is => 'rw' );
has at => ( is => 'ro' );
has colon => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core ) = @_;
  $core->_resolve_arguments(
    $self, [
      [ 'n' => 1 ],
    ]
  );
  $self->n( 1 ) unless defined $self->n;

  return "~" x $self->{n};
}

1;
