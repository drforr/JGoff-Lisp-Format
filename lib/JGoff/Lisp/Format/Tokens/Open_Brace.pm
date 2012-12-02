package JGoff::Lisp::Format::Tokens::Open_Brace;

use Moose;

has arguments => ( is => 'rw' );
has colon => ( is => 'ro' );
has at => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core, $operation, $close ) = @_;
  my $iteration_count;
  if ( $self->arguments ) {
    if ( $self->arguments->[0] eq '#' ) {
      $iteration_count = @{ $core->arguments };
    }
    elsif ( $self->arguments->[0] eq 'v' ) {
      $iteration_count = $core->current_argument;
      $core->forward_argument;
    }
    else {
      $iteration_count = $self->arguments->[0];
    }
  }

  my $output = '';
  if ( $core->current_argument and
       ref( $core->current_argument ) ) {
    for my $argument ( @{ $core->current_argument } ) {
      if ( defined $iteration_count ) {
        last if $iteration_count-- <= 0;
      }
      my $sub_self = $core->new(
        stream => $core->stream,
        tree => $operation,
        arguments => [ $argument ]
      );
      $output .= $sub_self->_format;
    }
  }
  elsif ( $core->current_argument ) {
    my $format = $core->current_argument;
    $core->forward_argument;
    for my $argument ( @{ $core->current_argument } ) {
      if ( defined $iteration_count ) {
        last if $iteration_count-- <= 0;
      }
      my $sub_self = $core->new(
        stream => $core->stream,
        format => $format,
        arguments => [ $argument ]
      );
      $output .= $sub_self->apply;
    }
  }
  return $output;
}

1;
