package JGoff::Lisp::Format::Tokens::Newline;

use Moose;

has at => ( is => 'ro', isa => 'Bool' );
has colon => ( is => 'ro', isa => 'Bool' );

=head1 NAME

JGoff::Lisp::Format::Tokens::Newline - Internal token for parser

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 METHODS

=head2 format( $core )

=cut

sub format {
  my $self = shift;
  return "\n" if $self->at;
  return "";
}

1;
