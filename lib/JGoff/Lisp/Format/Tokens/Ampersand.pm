package JGoff::Lisp::Format::Tokens::Ampersand;

use Moose;

sub format {
  my $self = shift;
  my ( $is_first, $before_percent ) = @_;
  if ( !$is_first and !$before_percent ) {
    return "\n";
  }

  return "";
}

1;
