package JGoff::Lisp::Format::Utils::Character;

use Moose;

has character => ( is => 'ro', isa => 'Str', required => 1 );

sub toString {
  my $self = shift;
  return $self->character;
}

1;
