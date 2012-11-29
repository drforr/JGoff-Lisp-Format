package JGoff::Lisp::Format::Tokens::Asterisk;

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

  if ( $self->{colon} ) {
    for ( 1 .. $self->{n} ) {
      $core->decrement_argument;
    }
  }
  elsif ( $self->{at} ) {
    $core->decrement_argument;
  }
  else {
    if ( defined $self->{n} and $self->{n} == 1 ) {
      $core->increment_argument;
    }
  }

  return "";
}

1;
