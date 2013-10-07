package JGoff::Lisp::Format::Tokens::P;

use Moose;

has colon => ( is => 'ro', isa => 'Bool' );
has at => ( is => 'ro', isa => 'Bool' );

=head1 NAME

JGoff::Lisp::Format::Tokens::P - Internal token for parser

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

  if ( $self->colon ) {
    $core->backward_argument;
    my $argument = $core->current_argument;
    $core->forward_argument;
    if ( $self->at ) {
      if ( defined( $argument ) and
           ( $argument eq 'No' or
             $argument == 0 ) ) {
        return 'ies';
      }
      elsif ( $argument and
              $argument == 1 ) {
        return 'y';
      }
      elsif ( $argument and
              $argument >= 2 ) {
        return 'ies';
      }
    }
    if ( defined( $argument ) and
         ( $argument eq 'No' or
           $argument == 0 ) ) {
      return 's';
    }
    elsif ( $argument and
            $argument == 1 ) {
      return '';
    }
    elsif ( $argument and
            $argument >= 2 ) {
      return 's';
    }
  }
  elsif ( $self->at ) {
    my $argument = $core->current_argument;
    $core->forward_argument;
    if ( defined( $argument ) and
         $argument == 0 ) {
      return 'ies';
    }
    elsif ( $argument and
            $argument == 1 ) {
      return 'y';
    }
    elsif ( $argument and
            $argument >= 2 ) {
      return 'ies';
    }
  }
  else {
    my $argument = $core->current_argument;
    $core->forward_argument;
    if ( defined( $argument ) and
         $argument == 0 ) {
      return 's';
    }
    elsif ( $argument and
            $argument == 1 ) {
      return '';
    }
    elsif ( $argument and
            $argument >= 2 ) {
      return 's';
    }
  }

  return '';
}

1;
