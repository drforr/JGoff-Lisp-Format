package JGoff::Lisp::Format::Role::Argument;

use Moose::Role;

has argument_id => ( is => 'rw', isa => 'Int', default => 0 );
has arguments => ( is => 'rw' );

sub first_argument {
  my $self = shift;
  return $self->arguments->[ 0 ]
    if $self->arguments;
  return undef;
}

sub previous_argument {
  my $self = shift;
  return $self->arguments->[ $self->argument_id - 1 ]
    if $self->arguments;
  return undef;
}

sub decrement_argument {
  my $self = shift;
  if ( $self->arguments ) {
    $self->argument_id( $self->argument_id - 1 );
    my $argument = $self->arguments->[ $self->argument_id ];
    return $argument;
  }
  return undef;
}

sub current_argument { $_[0]->arguments->[ $_[0]->argument_id ] }
sub num_arguments {
  my $self = shift;
  return 0 unless $self->arguments;
  return scalar @{ $self->arguments };
}

sub increment_argument {
  my $self = shift;
  if ( $self->arguments ) {
    my $argument = $self->arguments->[ $self->argument_id ];
    $self->argument_id( $self->argument_id + 1 );
    return $argument;
  }
  return undef;
}

sub next_argument {
  my $self = shift;
  return $self->arguments->[ $self->argument_id + 1 ]
    if $self->arguments;
  return undef;
}

1;
