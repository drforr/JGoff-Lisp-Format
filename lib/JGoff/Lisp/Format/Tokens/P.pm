package JGoff::Lisp::Format::Tokens::P;

use Moose;

has colon => ( is => 'ro' );
has at => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core ) = @_;

  if ( $self->{colon} ) {
    my $argument = $core->previous_argument;
    if ( $self->{at} ) {
      if ( defined $argument and ( $argument eq 'No' or $argument == 0 ) ) {
        return 'ies';
      }
      elsif ( $argument and $argument == 1 ) {
        return 'y';
      }
      elsif ( $argument and $argument >= 2 ) {
        return 'ies';
      }
    }
    if ( defined $argument and ( $argument eq 'No' or $argument == 0 ) ) {
      return 's';
    }
    elsif ( $argument and $argument == 1 ) {
      return '';
    }
    elsif ( $argument and $argument >= 2 ) {
      return 's';
    }
  }
  elsif ( $self->{at} ) {
    my $argument = $core->increment_argument;
    if ( defined $argument and $argument == 0 ) {
      return 'ies';
    }
    elsif ( $argument and $argument == 1 ) {
      return 'y';
    }
    elsif ( $argument and $argument >= 2 ) {
      return 'ies';
    }
  }
  else {
    my $argument = $core->increment_argument;
    if ( defined $argument and $argument == 0 ) {
      return 's';
    }
    elsif ( $argument and $argument == 1 ) {
      return '';
    }
    elsif ( $argument and $argument >= 2 ) {
      return 's';
    }
  }

  return '';
}

1;
