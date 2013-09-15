package JGoff::Lisp::Format::Tokens::Ampersand;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has n => ( is => 'rw' );

=head1 NAME

JGoff::Lisp::Format::Tokens::Ampersand - Internal token for parser

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 METHODS

=head2 format( $core )

=cut

sub format {
  my $self = shift;
  my ( $core, $is_first, $before_percent ) = @_;

  if ( $self->n and $self->n eq 'v' ) {
    $self->n( $core->current_argument );
    $core->forward_argument;
  }
  elsif ( $self->n and $self->n eq '#' ) {
    $self->n( $core->num_arguments );
  }

  return "\n" x $self->n
    if $self->n and $self->n > 0 and $is_first == 0 and $before_percent == 0;
  return "\n" x ( $self->n - 1 )
    if $self->n and $self->n > 0 and $is_first == 1 and $before_percent == 0;
  return "\n"
    if $is_first eq '' and $before_percent == 0;
  return "";
}

1;
