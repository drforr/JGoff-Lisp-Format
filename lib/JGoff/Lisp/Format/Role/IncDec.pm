package JGoff::Lisp::Format::Role::IncDec;

use Moose::Role;

has argument_id => ( is => 'rw', isa => 'Int', default => 0 );

sub increment_argument_id { $_[0]->argument_id( $_[0]->argument_id + 1 ) }
sub decrement_argument_id { $_[0]->argument_id( $_[0]->argument_id - 1 ) }

1;
