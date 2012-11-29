package JGoff::Lisp::Format::Tokens::Text;

use Moose;

has text => ( is => 'ro', isa => 'Str' );

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
