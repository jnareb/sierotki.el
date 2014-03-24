sierotki.el
===========

On-the-fly insertion of non-breakable spaces in (La)TeX modes
-------------------------------------------------------------

The purpose of this package is to connect some defined words (by default
one letter Polish prepositions) with the following words by tilde, which
is the non-breakable space in TeX.  This is needed to avoid one letter
prepositions at line endings in TeX documents, which is required by
the Polish and Czech typography/typesetting rules.

This program serves two purposes.  First of them is to check the text
and suggest adding missing tildes in some places.  This function is
implemented in `tex-hard-spaces' via `query-replace-regexp'.  It is
provided for convenience only to have both functionalities in the
same module.  More elaborated implementation can be found in the
`tildify' package which is part of GNU Emacs (ATTENTION: default
variable settings in the tildify package are suited for Czech
language, those here are for Polish).

The second purpose is the automatic, on-the-fly insertion of tildes
after one letter prepositions during writing.  It is implemented
via the `tex-magic-space' command which is a kind of electric space
and should be bound to SPC to work.  To activate this functionality
you have to turn on `tex-magic-space-mode'.  After loading this
package this command is bound to the `C-c SPC'.  The minor mode TeX
Magic Space can be also turned on from the modeline minor mode
menu.  This mode is denoted by " ~" in the modeline.  The ":Chk"
after " ~" in the modeline shows that test are enabled.  You can
enable tests using `tex-magic-space-toggle-checking' command, bound to the
`C-c C-SPC'.

For the time being the tests in `tex-magic-space-tests' are in early beta
phase; if you want to insert ` ' where `tex-magic-space-mode' inserts
`~', use `C-q SPC' to enter single space, or turn off the TeX Magic Space
mode fro editing the fragment of document where nonbreakable spaces are
not needed.

The TeX Magic Space mode can be automatically turned on in the TeX modes
by adding the equivalent of `turn-on-tex-magic-space-mode' to the
hooks defined in the variable `tex-magic-space-mode-hooks-list' using
the command `turn-on-tex-magic-space-in-tex-modes'.

There are also defined two fill predicates,
`fill-single-letter-word-nobreak-p' and `fill-tex-magic-space-nobreak-p',
which after set as value of `fill-nobreak-predicate' variable makes
filling (`M-q' aka `fill-paragraph' and `auto-fill-mode') to not break
line after single letter words.  The latter predicate uses the same test
as TeX Magic Space mode.  Not shown in modeline.


See also: http://www.emacswiki.org/cgi-bin/wiki/NonbreakableSpace
