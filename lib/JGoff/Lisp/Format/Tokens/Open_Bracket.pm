package JGoff::Lisp::Format::Tokens::Open_Bracket;

use Moose;

has arguments => ( is => 'rw' );
has colon => ( is => 'ro' );
has at => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core, $operation, $close ) = @_;
  my $iteration_count = undef;

  if ( $self->arguments and defined( $self->arguments->[0] ) ) {
    my $first_argument = $self->arguments->[0];
    if ( $first_argument eq '#' ) {
      $iteration_count = $core->num_arguments;
    }
    elsif ( $first_argument eq 'v' ) {
      $iteration_count = $core->current_argument;
      $core->forward_argument;
    }
    else {
      $iteration_count = $first_argument;
    }
  }

  my $output = '';
  if ( $core->current_argument and
       ref( $core->current_argument ) and
       ref( $core->current_argument ) eq 'CODE' ) {
    my $fn = $core->current_argument;
    $core->forward_argument;
    for my $argument ( @{ $core->remaining_arguments } ) {
      if ( defined $iteration_count ) {
        last if $iteration_count-- <= 0;
      }
      $output .= $fn->( $core->stream, [ $argument ] );
    }
  }
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
  else {
#    if ( $close->colon and
#         ( ( defined( $iteration_count ) and $iteration_count != 0 ) or
#           ( !defined( $iteration_count ) ) ) ) {
#      my $sub_self = $core->new(
#        stream => $core->stream,
#        tree => $operation,
#        arguments => undef
#      );
#      $output .= $sub_self->_format;
#    }
  }
  return $output;
}

1;
