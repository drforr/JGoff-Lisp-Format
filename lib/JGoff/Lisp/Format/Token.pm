package JGoff::Lisp::Format::Token;

use Moose;
use Readonly;
use Carp qw( croak );

Readonly our $upcase => 'upcase';
Readonly our $downcase => 'downcase';
Readonly our $capitalize => 'capitalize';

=head1 NAME

JGoff::Lisp::Format::Token - Internal token superclass

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 METHODS

=cut

# {{{ _print_case( $print_case, $argument )

sub _print_case {
  my $self = shift;
  my ( $print_case, $argument ) = @_;
  if ( $print_case eq $JGoff::Lisp::Format::Token::upcase ) {
    if ( ref( $argument ) and
         ref( $argument ) eq 'JGoff::Lisp::Format::Utils::Character' ) { # XXX
      return $argument;
    }
    return uc( $argument );
  }
  elsif ( $print_case eq $JGoff::Lisp::Format::Token::downcase ) {
    return lc( $argument );
  }
  elsif ( $print_case eq $JGoff::Lisp::Format::Token::capitalize ) {
    return ucfirst( lc( $argument ) );
  }
  else {
    croak "Unknown or missing print_case '" . $print_case . "'";
  }
}

# }}}

# {{{ _argument_to_base( $base, $argument )

sub _argument_to_base {
  my $self = shift;
  my ( $base, $argument ) = @_;

  if ( $base != 10 ) {
    my @radix = ( '0' .. '9', 'a' .. 'z' ); # Yes, handles up to base 36
    my $digits = '';
    while ( $argument > 0 ) {
      my $digit = $argument % $base;
      $digits = $radix[ $digit ] . $digits;
      $argument -= $digit;
      $argument /= $base;
    }
    $argument = $digits;
  }
  $argument = $self->_commify( $argument );
  $argument = $self->_padding( $argument );
  return $argument;
}

# }}}

# {{{ _resolve_parameters( $core, $tuples )

sub _resolve_parameters {
  my $self = shift;
  my ( $core, $tuples ) = @_;

  if ( $self->parameters ) {
    for my $tuple ( @$tuples ) {
      my ( $name, $default ) = @$tuple;
      my $value = shift @{ $self->parameters };
      if ( defined $value ) {
        $self->{$name} = $value;
      }
    }
    delete $self->{parameters};
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
  if ( $mincol > length( $argument ) and
       $colinc ) {
    while ( length( $argument ) + length( $padding ) < $mincol ) {
      $padding .= $padchar x $colinc;
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
    #unshift @chunk, $argument if $argument and $argument ne '';
    unshift @chunk, $argument if defined $argument;
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

=head2 char_name( $char )

=cut

# {{{ char_name( $char )

sub char_name {
  my $self = shift;
  my ( $char ) = @_;
  return 'Space' if $char eq ' ';
  return $char;
}

# }}}

1;
