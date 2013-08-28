package JGoff::Lisp::Format::Tokens::Text;

use Moose;

has text => ( is => 'ro', isa => 'Str' );

=head1 NAME

JGoff::Lisp::Format::Tokens::Text - Internal token for parser

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 METHODS

=head2 format( $core )

=cut

sub format {
  my $self = shift;
  my ( $before_newline, $nl_colon ) = @_;
  my $text = $self->text;

  if ( $before_newline and !$nl_colon ) {
    $text =~ s{ ^ \s+ }{}x;
  }
  return $text;
}

1;
