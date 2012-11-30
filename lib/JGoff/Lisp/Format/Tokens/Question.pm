package JGoff::Lisp::Format::Tokens::Question;

use Moose;

has at => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core ) = @_;

  if ( $self->at ) {
    my $format = $core->increment_argument;
    my @remaining_arguments =
      @{ $core->arguments }[ $core->argument_id .. $#{ $core->arguments } ];
    my $arguments = $core->increment_argument;
    my $sub_self = $core->new(
      stream => $core->stream,
      format => $format,
      arguments => [ @remaining_arguments ]
    );
    return $sub_self->apply;
  }
  else {
    my $format = $core->increment_argument;
    my $arguments = $core->increment_argument;
    my $sub_self = $core->new(
      stream => $core->stream,
      format => $format,
      arguments => $arguments
    );
    return $sub_self->apply;
  }
  return '';
}

# }}}
1;
