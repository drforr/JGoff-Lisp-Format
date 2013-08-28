package JGoff::Lisp::Format::Tokens::Ampersand;

use Moose;

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
  my ( $is_first, $before_percent ) = @_;
  if ( !$is_first and !$before_percent ) {
    return "\n";
  }

  return "";
}

1;
