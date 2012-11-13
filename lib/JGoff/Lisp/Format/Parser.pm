package JGoff::Lisp::Format::Parser;

use Moose;

extends 'Parser::MGC';

=head1 NAME

JGoff::Lisp::Format::Parser - Parser internals

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

Quick summary of what the module does.

    use JGoff::Lisp::Format::Parser;

    my $foo = JGoff::Lisp::Format->new();
    ...

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.

=head1 METHODS

=head2 parse( $text )

=cut

sub __token_a {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [:] | v | v[:] | [@] | v[@] | v[:][@] | v[@][:] | \d+,\d+ |
            \d+,\d+[@] | v,,\d+ | \d+,,[-+]\d+ | \d+,,\d+ |
            \d+,,,'X | \d+,,, | \d+,,,'X | \d+,,,'X[@] | \d+,,,[@] | \d+,,,v |
            \d+,,,v[@] | \d+,,v | \d+,,v[@] | \d+,v | \d+,v[@] | [#] | [#][@] |
            \d+,[#] | \d+,[#][@] | [#],[#] | [#],[#][@] | -\d+
      )
    [aA]
  }x );
  my $rv = {
    format => '~a',
    mincol => 0,
    minpad => 0,
    colinc => 1,
    padchar => ' '
  };
  return $rv;
}

sub __token_ampersand {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | \d+ | [vV]
      )
    [&]
  }x );
  my $rv = {
    format => '~&',
  };
  return $rv;
}

sub __token_percent {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [vV] | [#]
      )
    [%]
  }x );
  my $rv = {
    format => '~%',
  };
  return $rv;
}

sub __token_b {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [:] | v | \d+,v | ,,v[:] | ,,'[*],v[:] | [-+]\d+ | [+]\d+[@]
      )
    [bB]
  }x );
  my $rv = {
    format => '~b',
  };
  return $rv;
}

sub __token_c {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?:
      )
    [cC]
  }x );
  my $rv = {
    format => '~c',
  };
  return $rv;
}

sub __token_open_brace {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | \d+ | [#] | [Vv] | [:] | \d+[:] | [Vv][:] | [#][:] | [@] |
            \d+[@] | [:][@] | [@][:] | v[@] | [#][@] | \d+[#][@] |
            \d+[:][@] | [#][:][@] | v[:][@]
      )
    \{
  }x );
  my $rv = {
    format => '~{',
  };
  return $rv;
}

sub __token_newline {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [@]
      )
    \n
  }x );
  my $rv = {
    format => '~\\n',
  };
  return $rv;
}

sub __token_close_brace {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [:]
      )
    \}
  }x );
  my $rv = {
    format => '~}',
  };
  return $rv;
}

sub __token_circumflex {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | v | [#],\d+ | \d+,[#] | [#],[#] | [#],[#],v | \d+,v,v | \d+,v |
            \d+,\d+,v | 'X | ',,', | v,\d+,v | v,v,v | v,[#] | v,\d+ | 'x,\d+ |
            \d+,'x | v,\d+,\d+ | \d+,v,\d+ | [:] | 'X [:] | [vV][:] |
            \d+,\d+ [:] | v,\d+ [:] | \d+,V [:] | V,v [:] | [#],\d+[:] |
            \d+,[#][:] | [#],[#][:] | [#],v[:] | V,[#][:] | 'X,'Y[:] |
            'X,'X[:] | \d+,\d+,v | \d+,\d+,v[:] | v,\d+,\d+[:] |
            \d+,v,\d+ | \d+,v,\d+[:] | V,v,\d+[:] | v,\d+,v[:] |
            \d+,V,v[:] | v,v,V[:] |

            ( [#] | \d+ ),( [#] | \d+ ),( [#] | \d+ ) [:]? | # 1,1,1 .. #,#,#
            ( v | '[xX] ),( v | '[xX] ) |                    # v,v .. 'x,'x
            ( [#] | \d+ ) [:]?
      )
    \^
  }x );
  my $rv = {
    format => '~^',
  };
  return $rv;
}

sub __token_colon {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [@]
      )
    [:]
  }x );
  my $rv = {
    format => '~:',
  };
  return $rv;
}

sub __token_question {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [@]
      )
    [?]
  }x );
  my $rv = {
    format => '~?',
  };
  return $rv;
}

sub __token_open_bracket {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | \d+ | -\d+ | [#] | [@]
      )
    \[
  }x );
  my $rv = {
    format => '~[',
  };
  return $rv;
}

sub __token_close_bracket {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?:
      )
    \]
  }x );
  my $rv = {
    format => '~]',
  };
  return $rv;
}

sub __token_open_paren {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [@]
      )
    \(
  }x );
  my $rv = {
    format => '~(',
  };
  return $rv;
}

sub __token_close_paren {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?:
      )
    \)
  }x );
  my $rv = {
    format => '~)',
  };
  return $rv;
}

sub __token_semicolon {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [:]
      )
    [;]
  }x );
  my $rv = {
    format => '~;',
  };
  return $rv;
}

sub __token_d {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [v] | \d+,v | ,,v[:] | ,,'\*,v[:] | [+]\d+[:] | [+]\d+[@] |
          [-+]\d+
      )
    [dD]
  }x );
  my $rv = {
    format => '~d',
  };
  return $rv;
}

sub __token_f {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | ,,,,',
      )
    [fF]
  }x );
  my $rv = {
    format => '~f',
  };
  return $rv;
}

sub __token_asterisk {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | \d+ | [vV] [:] | [vV][@] | [vV] | [:] | \d+[:] | \d+[@] | \d+[vV][@] | [@]
      )
    [*]
  }x );
  my $rv = {
    format => '~*',
  };
  return $rv;
}

sub __token_o {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [vV] | \d+,[vV] | ,,v[:] | ,,'[*],v[:] | [+]\d+[@] | [+]\d+
          | [-]\d+
      )
    [oO]
  }x );
  my $rv = {
    format => '~o',
  };
  return $rv;
}

sub __token_p {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [:] | [@] | [@][:] | [:][@]
      )
    [pP]
  }x );
  my $rv = {
    format => '~p',
  };
  return $rv;
}

sub __token_pipe {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | \d+ | [vV]
      )
    [|]
  }x );
  my $rv = {
    format => '~|',
  };
  return $rv;
}

sub __token_r {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | \d+ | [vV] | [#] | \d+[@] | \d+[:] | \d+[@][:] | \d+,,,,\d+
          | \d+,\d+[:][@] | \d+,\d+,,'[*][:] | \d+,\d+,'X,',[:] | \d+,[vV]
          | \d+,[#] | \d+,\d+,[vV] | \d+,,,,[vV][:] | \d+,,,[vV][:]
          | \d+,,,,[#][:] | \d+,,,,\d+[:] | [+]\d+ | \d+,[+]\d+ | \d+,\d+
          | \d+,[-]\d+
      )
    [rR]
  }x );
  my $rv = {
    format => '~r',
  };
  return $rv;
}

sub __token_s {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [vV] | [vV][:] | [@] | [vV][@] | [vV][:][@] | [vV][@][:] | \d+,\d+
          | \d+,\d+[@] | \d+,,[+]\d+ | \d+,,\d+ | \d+,,[-]\d+ | \d+,,,'X
          | \d+,,, | \d+,,,'X[@] | \d+,,,[@] | \d+,,,[vV] | \d+,,,[v][@]
          | \d+,,[vV] | \d+,[vV] | \d+,,[vV][@] | \d+,[vV][@]
      )
    [sS]
  }x );
  my $rv = {
    format => '~s',
  };
  return $rv;
}

sub __token_x {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [vV] | \d+,[vV] | ,,[vV][:] | ,,'[*],[vV][:] | [+]\d+ | [+]\d+[@]
          | [-]\d+
      )
    [xX]
  }x );
  my $rv = {
    format => '~x',
  };
  return $rv;
}

sub __token_tilde {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [vV]
      )
    ~
  }x );
  my $rv = {
    format => '~~',
  };
  return $rv;
}

sub parse {
  my $self = shift;

  $self->sequence_of( sub {
    $self->any_of(
      sub { $self->expect( qr{
        [!][@][#]0[^][&][*]this |
        ABC | cat | penn | XXyy | uuVV | this | is | TEST[.]
            | [a-zA-Z()] | NO | FOO | XYZ | \[ | \]
            | [,]
      }x ) },
      sub { $self->__token_p },
      sub { $self->__token_asterisk },
      sub { $self->__token_semicolon },
      sub { $self->__token_a },
      sub { $self->__token_ampersand },
      sub { $self->__token_percent },
      sub { $self->__token_b },
      sub { $self->__token_open_brace },
      sub { $self->__token_open_bracket },
      sub { $self->__token_c },
      sub { $self->__token_open_paren },
      sub { $self->__token_newline },
      sub { $self->__token_close_brace },
      sub { $self->__token_close_bracket },
      sub { $self->__token_close_paren },
      sub { $self->__token_circumflex },
      sub { $self->__token_colon },
      sub { $self->__token_d },
      sub { $self->__token_question },
      sub { $self->__token_f },
      sub { $self->__token_asterisk },
      sub { $self->__token_o },
      sub { $self->__token_pipe },
      sub { $self->__token_r },
      sub { $self->__token_s },
      sub { $self->__token_tilde },
      sub { $self->__token_x },
    );
  } );
}

=head1 AUTHOR

Jeff Goff, C<< <jgoff at cpan.org> >>

 =head1 BUGS

Please report any bugs or feature requests to C<bug-jgoff-lisp-format at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=JGoff-Lisp-Format>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc JGoff::Lisp::Format


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=JGoff-Lisp-Format>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/JGoff-Lisp-Format>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/JGoff-Lisp-Format>

=item * Search CPAN

L<http://search.cpan.org/dist/JGoff-Lisp-Format/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2012 Jeff Goff.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1; # End of JGoff::Lisp::Format
