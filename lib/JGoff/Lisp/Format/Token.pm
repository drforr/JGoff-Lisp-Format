package JGoff::Lisp::Format::Token;

use Moose;
use Readonly;
use Carp qw( croak );

Readonly our $upcase => 'upcase';
Readonly our $downcase => 'downcase';
Readonly our $capitalize => 'capitalize';

# {{{ _print_case( $argument )

sub _print_case {
  my $self = shift;
  my ( $core, $argument ) = @_;
  if ( $core->print_case eq $JGoff::Lisp::Format::Token::upcase ) {
    if ( ref( $argument ) and
         ref( $argument ) eq 'JGoff::Lisp::Format::Utils::Character' ) { # XXX
      return $argument;
    }
    return uc( $argument );
  }
  elsif ( $core->print_case eq $JGoff::Lisp::Format::Token::downcase ) {
    return lc( $argument );
  }
  elsif ( $core->print_case eq $JGoff::Lisp::Format::Token::capitalize ) {
    return ucfirst( lc( $argument ) );
  }
  else {
    croak "Unknown or missing print_case '" . $core->print_case . "'";
  }
}

# }}}

# {{{ _argument_to_base( $base, $core )

sub _argument_to_base {
  my $self = shift;
  my ( $base, $core ) = @_;
  my $argument = $core->current_argument;
  $core->forward_argument;

  my @radix = ( '0' .. '9', 'a' .. 'z' ); # Yes, handles up to base 36
  my $digits = '';

  if ( $base != 10 ) {
    while ( $argument > 0 ) {
      my $digit = $argument % $base;
      $digits = $radix[ $digit ] . $digits;
      $argument -= $digit;
      $argument /= $base;
    }
  }
  else {
    $digits = $argument;
  }
  $digits = $self->_commify( $digits );
  $digits = $self->_padding( $digits );
  return $digits;
}

# }}}

# {{{ _resolve_arguments( $core, $tuples )

sub _resolve_arguments {
  my $self = shift;
  my ( $core, $tuples ) = @_;

  if ( $self->arguments ) {
    for my $tuple ( @$tuples ) {
      my ( $name, $default ) = @$tuple;
      my $value = shift @{ $self->arguments };
      if ( defined $value ) {
        $self->{$name} = $value;
      }
    }
    delete $self->{arguments};
  }
  for my $tuple ( @$tuples ) {
    my ( $name, $default ) = @$tuple;
    if ( defined $self->{$name} and $self->{$name} eq 'v' ) {
      $self->{$name} = $core->current_argument;
      $core->forward_argument;
      $self->{"$name-v"} = 1;
    }
    elsif ( defined $self->{$name} and $self->{$name} eq '#' ) {
      $self->{$name} = $core->num_arguments;
    }
    if ( !defined( $self->{$name} ) ) {
      $self->{$name} = $default;
    }
    if ( $name =~ /char/ ) {
      $self->{$name} =~ s{^'(.)}{$1};
    }
  }
}

# }}}

# {{{  _padding( $argument )

sub _padding {
  my $self = shift;
  my ( $argument ) = @_;
  my ( $padchar, $minpad, $mincol, $colinc ) =
    @{ $self }{qw( padchar minpad mincol colinc )};

  my $padding = '';
  if ( $minpad and $minpad > 0 ) {
    $padding .= $padchar x $minpad;
  }
  if ( $padchar and $padchar =~ /./ ) {
    if ( $mincol and $mincol > length( $argument ) ) {
      if ( $colinc and $colinc > 0 ) {
        while ( length( $argument ) + length( $padding ) < $mincol ) {
          $padding .= $padchar x $colinc;
        }
      }
    }
  }

  if ( $self->at ) {
    $argument = $padding . $argument;
  }
  else {
    $argument = $argument . $padding;
  }

  if ( $argument and
       $mincol and
       length( $argument ) < $mincol ) {
    $argument = ' ' x ( $mincol - length( $argument ) ) . $argument;
  }
  return $argument;
}

# }}}

# {{{ _commify( $argument )

sub _commify {
  my $self = shift;
  my ( $argument ) = @_;
  my $interval = $self->{'comma-interval'};
  my $commachar = $self->{commachar};
  my $sign = 1;

  if ( $argument and $argument !~ /[^-+0-9.]/ and $argument < 0 ) {
    $sign = -1;
    $argument = abs( $argument );
  }
  if ( $self->colon ) {
    my @chunk;
    while ( $argument and
            length( $argument ) > $interval ) {
      unshift @chunk, substr( $argument, -$interval, $interval, '' );
    }
    unshift @chunk, $argument if $argument and $argument ne '';
    $argument = join $commachar, @chunk;
  }
  if ( $sign < 0 ) {
    return '-' . $argument;
  }
  elsif ( $sign > 0 and $self->at ) {
    return '+' . $argument;
  }
  return $argument;
}

# }}}

# {{{ char_name 

sub char_name {
  my $self = shift;
  my ( $char ) = @_;
  return 'Space' if $char eq ' ';
  return $char;
}

# }}}

1;
