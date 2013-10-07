package JGoff::Lisp::Format::Tokens::X;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has arguments => ( is => 'rw' );
has colon => ( is => 'ro', isa => 'Bool' );
has at => ( is => 'ro', isa => 'Bool' );

=head1 NAME

JGoff::Lisp::Format::Tokens::X - Internal token for parser

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 METHODS

=head2 format( $core )

=cut

sub format {
  my $self = shift;
  my ( $core ) = @_;
  $self->_resolve_arguments(
    $core, [
      [ 'mincol' => 0 ],
      [ 'padchar' => ' ' ],
      [ 'commachar' => ',' ],
      [ 'comma-interval' => 3 ],
    ]
  );

  my $argument = $core->current_argument;
  $core->forward_argument;
  return $self->_argument_to_base( 16, $argument );
}

1;
