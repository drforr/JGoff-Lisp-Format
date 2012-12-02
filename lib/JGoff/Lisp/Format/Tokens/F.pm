package JGoff::Lisp::Format::Tokens::F;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has arguments => ( is => 'rw' );
has colon => ( is => 'ro' );
has at => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core ) = @_;
  $self->_resolve_arguments(
    $core, [
      [ w => 0 ],
      [ d => 0 ],
      [ k => 0 ],
      [ overflowchar => ',' ],
      [ padchar => ',' ],
    ]
  );

  my $argument = $core->current_argument;
  $core->forward_argument;
  $argument = sprintf "%f", $argument;
  if ( $argument =~ m{ [.] [0]+ $ }x ) {
    $argument =~ s{ [.] [0]+ $ }{.0}x;
  }

  $argument = $self->_commify( $argument );

  if ( $argument and
       $self->{w} > 0 and
       length( $argument ) < $self->{w} ) {
    $argument = ' ' x ( $self->{w} - length( $argument ) ) . $argument;
  }

  return $argument;
}

1;
