package JGoff::Lisp::Format::Tokens::Newline;

use Moose;

has at => ( is => 'ro' );
has colon => ( is => 'ro' );

sub format {
  my $self = shift;
  return "\n" if $self->at;
  return "";
}

1;
