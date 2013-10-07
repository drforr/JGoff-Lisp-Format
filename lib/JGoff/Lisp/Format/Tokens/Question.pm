package JGoff::Lisp::Format::Tokens::Question;

use Moose;

has at => ( is => 'ro', isa => 'Bool' );

=head1 NAME

JGoff::Lisp::Format::Tokens::Question - Internal token for parser

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

  if ( $self->at ) {
    my $format = $core->current_argument;
    $core->forward_argument;
    my @remaining_arguments =
      @{ $core->arguments }[ $core->argument_index .. $#{ $core->arguments } ];
    my $arguments = $core->current_argument;
    $core->forward_argument;
    my $sub_self = $core->new(
      stream => $core->stream,
      format => $format,
      arguments => [ @remaining_arguments ]
    );
    return $sub_self->apply;
  }
  else {
    my $format = $core->current_argument;
    $core->forward_argument;
    my $arguments = $core->current_argument;
    $core->forward_argument;
    my $sub_self = $core->new(
      stream => $core->stream,
      format => $format,
      arguments => $arguments
    );
    return $sub_self->apply;
  }
  return '';
}

1;
