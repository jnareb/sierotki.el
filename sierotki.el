;;; sierotki.el --- Introduce tildes after single-letter words

;; Copyright (C) 1999-2003  Michal Jankowski, Jakub Narebski

;; Authors: 	Ryszard Kubiak   <rysiek@ipipan.gda.pl>
;;		Micha³ Jankowski <michalj@fuw.edu.pl>
;;		Jakub Narebski   <jnareb@fuw.edu.pl>
;; Maintainer: 	Jakub Narebski <jnareb@fuw.edu.pl>
;; Version: 	2.6.6
;; RCS version:	$Revision$
;; Date: 	$Date$
;; Keywords: 	TeX, wp, convenience
;; Created: 	03-11-1999
;; URL: 	http://www.fuw.edu.pl/~jnareb/sierotki.el
;;
;; Compatibility:   Emacs21, XEmacs21
;; Incompatibility:

;; $Id$

;; This file is *NOT* part of GNU Emacs.
;; This file is distributed under the same terms as GNU Emacs.

;;{{{ GPL

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; http://www.fsf.org/copyleft/gpl.html

;;}}}

;;; Commentary:

;;; Installation:

;; To use this package, put the following line in your .emacs:
;;
;;    (require 'sierotki)
;;
;; If you do not want to load this package before it is necessary, you
;; can make use of the `autoload' feature, e.g. adding to your .emacs
;; the following lines
;;
;;    (autoload 'tex-magic-space-mode "sierotki"
;;              "TeX Magic Space minor mode" t)
;;    (define-key mode-specific-map " " 'tex-magic-space-mode)
;;
;; Then after turning on `tex-magic-space-mode' via `C-c SPC'
;; the whole package will be loaded.  Attention: using autoload means
;; that this mode _can't_ be turned on automatically in LaTeX modes.
;;
;; If you want to have the TeX Magic Space mode turned on in known
;; TeX modes put the following line in your .emacs after (require 'sierotki)
;;
;;    (turn-on-tex-magic-space-in-tex-modes)


;;; Description:

;; The purpose of this package is to connect some defined words (by default
;; one letter Polish prepositions) with the following words by tilde, which
;; is the non-breakable space in TeX.  This is needed to avoid one letter
;; prepositions at line endings in TeX documents, which is required by
;; the Polish and Czech ortography/typography rules.
;;
;; This program serves two purposes.  First of them is to check the text
;; and suggest adding missing tildes in some places.  This function is
;; implemented in `tex-hard-spaces' via `query-replace-regexp'.  It is
;; provided for convenience only to have both functionalities in the
;; same module.  More elaborated implementation can be found in the
;; `tildify' package which is part of GNU Emacs (ATTENTION: default
;; variable settings in the tildify package are suited for Czech
;; language, those provided here are for Polish).
;;
;; The second purpose is the automatic, in-the-fly insertion of tildes
;; after one letter prepositions during writing.  It is implemented
;; via the `tex-magic-space' command which is a kind of electric space
;; and should be bound to SPC to work.  To activate this functionality
;; you have to turn on `tex-magic-space-mode'.  The minor mode TeX Magic
;; Space can be turned on from the modeline minor mode menu.  This mode
;; is denoted by " ~" in the modeline.  The ":Chk" after " ~" in the modeline
;; shows that test are enabled.
;;
;; The TeX Magic Space mode can be automatically turned on in the TeX modes
;; by adding the equivalent of `turn-on-tex-magic-space-mode' to the
;; hooks defined in the variable `tex-magic-space-mode-hooks-list' using
;; the command `turn-on-tex-magic-space-in-tex-modes'.

;; Documentation and comments: Jakub Narebski.


;;; Change Log:

;; Version 1.2 (RCS revision 1.2):
;; * Added `tex-toggle-magic-space'.
;; Version 1.3 (RCS revision 1.4):
;; * Regexps in variables and not hardcoded.
;; Version 2.0 (RCS revision 1.6):
;; * New implementation of `tex-magic-space'.
;; Version 2.3 (RCS revision 1.12):
;; * TeX Magic Space minor mode (bound to `C-c SPC')
;; Version 2.4 (RCS revision 1.17):
;; * Added checking if the `tex-magic-space' should be active or not
;;   (e.g. it should be inactive in math mode detected using `texmathp').
;;   It was implemented using advices.
;; Version 2.5 (RCS revision 1.26):
;; * Removed `tex-toggle-magic-space'; use `tex-magic-space-mode' instead.
;; Version 2.6 (RCS revision 1.31):
;; * Checking if `tex-magic-space' should be active was changed from
;;   the around advice(s) to the conditional in main function.
;; Version 2.6.6 (RCS revision 1.37):
;; * Removed Polish comments.

;;; Code:


;;;; ======================================================================
;;;; Add non-breakable spaces in existing document, interactively.

;;; Hard spaces by Ryszard Kubiak <rysiek@ipipan.gda.pl>
;;; Modified by Jakub Narebski <jnareb@fuw.edu.pl>

(defvar tex-hard-spaces-regexp "\\<\\([aeiouwzAEIOUWZ]\\)\\s +"
  "*Regular expression which detects single [aeiouwz] for `tex-hard-spaces'.
The part of regular expression which matches string to be saved
should be in parentheses, so the replace part \\\\1~ will work.

Used as first argument to `query-replace-regexp'.")

(defun tex-hard-spaces ()
  "Replace whitespace characters after single-letter word with `~'.
Replaces whitespace characters following single-letter conjunctions by `~',
the TeX non-breakable space in whole buffer, interactively.
Uses `tex-hard-spaces-regexp' for single-letter conjunctions detection.

It can be used to bind single-letter conjunction to the word following it in
the existing text, using `~' (the TeX non-breakable space), so there are no
single-letter conjunctions at the end of the line (known as 'orphans').

For on-the-fly 'tildification' turn on TeX Magic Space minor mode using
command \\[tex-magic-space-mode].

It is implemented using `query-replace-regexp'."
 (interactive)
 (query-replace-regexp tex-hard-spaces-regexp
                       "\\1~"))


;;;; ======================================================================
;;;; On-the-fly inserting of non-breakable spaces.

;;; Magic space by Michal Jankowski <michalj@fuw.edu.pl>
;;; Modified by Jakub Narebski <jnareb@fuw.edu.pl>


;;; ----------------------------------------------------------------------
;;; Tests for `tex-magic-space'

;; Workaround for XEmacs (not needed anymore?)
;(unless (fboundp 'match-string-no-properties)
;  (defalias 'match-string-no-properties 'match-string))

(defun texinverbp ()
  "Determine if point is inside LaTeX \\verb command.
Returns nil or the pair (POINT-VERB-BEG . POINT-VERB-END) of positions where
\\verb argument begins and ends or the position POINT-VERB-BEG where \\verb
command argument begins if \\verb is unfinished (has no closing delimiter).

This command uses the fact that the argument to \\verb cannot contain end of
line characters.  Does not work with nested \\verbs."
  (interactive)
  (let ((point (point))
	beg
	end
	delim)
  (save-excursion
    (and (setq beg (and (re-search-backward "\\\\verb\\*?\\([^a-zA-Z*\\n]\\)"
					   (point-at-bol) t)
		       (match-end 0)))
	 (setq delim (regexp-quote (match-string 1)))
	 (goto-char beg)
	 ;;(or (insert "!") t)        ; debug
	 (setq end (and (skip-chars-forward (concat "^" delim)
					    (point-at-eol))
			(point)))
	 (or (eolp)
	     (looking-at (concat "[" delim "]")))
	 ;;(or (insert "!") t)        ; debug
	 (cond ((>= point end) nil)
	       ((eolp) beg)
	       (t (cons beg end)))))))

;;; ......................................................................
;;; Turning on tests for tex-magic-space
(defvar tex-magic-space-do-checking nil
  "*Non-nil if `tex-magic-space' checks `tex-magic-space-tests'.

Set by `tex-magic-space-toggle-checking'")

(defvar tex-magic-space-tests
  (list
   (unless (and (boundp 'running-xemacs) running-xemacs) 'texinverbp)
   (if (or (featurep 'tex-site) (fboundp 'texmathp)) 'texmathp))
  "List of test functions for `tex-magic-space'.

List of functions which are invoked, in order, to determine whether
`tex-magic-space' could insert a ~ (i.e., a tex non-breakable
space).  The tilde can be inserted only when every function returns
a nil value.  The tests are run only when `tex-magic-space-do-checking'
has non-nil value")


;;; ----------------------------------------------------------------------
;;; On-the-fly tildes insertion

(defvar tex-magic-space-regexp "\\<[aeiouwzAEIOUWZ]\\'"
  "*Regular expression which detects single [aeiouwz] for `tex-magic-space'.
`tex-magic-space' inserts `~' if this expression matches two characters before
point, otherwise it inserts the key it is bound to (\\[tex-magic-space]),
usually SPC.

This regular expression should end with [aeiouwzAEIOUWZ]\\\\' to match possible
single letter conjunction against the letter directly before the point.  The
part before [aeiouwzAEIOUWZ] should match word beginning/boundary.

ATTENTION: sometimes in unibyte mode the non US-ASCII letters are considered
word boundary, even when they are word constituents.")


(defun tex-magic-space (&optional prefix)
  "Magic-space - insert non-breakable space after a single-letter word.
Interactively, PREFIX is the prefix arg (default 1).
Uses `tex-magic-space-regexp' for single-letter words detection.

Works well with auto filling unless `~' is in the table `auto-fill-chars',
in which case `~' is inserted but might be followed by line break.
Works with abbrev expansion with the following exceptions:
 - doesn't do abbrev expansion if abbrev is single letter word
   and `~' is word constituent (according to current syntax table)
 - abbrevs ending with single-letter word will have `~' instead of space
   after the expansion
 - abbrevs with expansion ending with single-letter word won't have
   the SPC following single-letter word substituted with `~'

Should not be used directly.

To use it turn on TeX Magic Space minor mode using command
`tex-magic-space-mode' (\\[tex-magic-space-mode]).

See also: `tex-hard-spaces'"
  (interactive "p")
  ;; Tests
  (unless (and tex-magic-space-do-checking
	       (some (lambda (f) (and (fboundp f) (funcall f)))
		     tex-magic-space-tests))
    ;; tests failed
    (when (string-match
	   tex-magic-space-regexp
	   (buffer-substring (max (point-min) (- (point) 2)) (point)))
      (setq last-command-char ?~)))
  (self-insert-command (or prefix 1)))

(defun debug-tex-magic-space (&optional prefix)
  "Version of `tex-magic-space' which does'n do any testing."
  (interactive "p")
  (let ((tex-magic-space-do-checking nil))
    (tex-magic-space prefix)))


;;; ----------------------------------------------------------------------
;;; The TeX Magic Space mode definition and initialization


(defvar tex-magic-space-mode nil
  "*Determines if TeX Magic Space mode is active.
You can set it directly or use the command `tex-magic-space-mode'.")
(make-variable-buffer-local 'tex-magic-space-mode)

(defvar tex-magic-space-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'tex-magic-space)
    (if (and (boundp 'running-xemacs) running-xemacs)
	(define-key map [(control c) (control space)] 'tex-magic-space-toggle-checking)
      (define-key map [?\C-c ?\C- ] 'tex-magic-space-toggle-checking))
    map)
  "Keymap for TeX Magic Space mode.")

;;;###autoload
(defun turn-on-tex-magic-space-mode ()
  "Turn on TeX Magic Space mode.
Does not force the modeline update."
  (setq tex-magic-space-mode t))

;;;###autoload
(defun tex-magic-space-mode (&optional arg)
  "Toggle TeX Magic Space mode.

With ARG, turn TeX Magic Space mode on if and only if ARG is positive.
In TeX Magic Space mode typing a space inserts tilde, the TeX non-breakable
space, after single-letter prepositions described by `tex-magic-space-regexp'
if we are not in one of situations described by `tex-magic-space-tests'.
The testing can be toggled using `\\[tex-magic-space-toggle-checking]' which
runs `tex-magic-space-toggle-checking'.
 
\\<tex-magic-space-mode-map>"
  (interactive "P")
  (setq tex-magic-space-mode
	(if (null arg) (not tex-magic-space-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))


(defun tex-magic-space-toggle-checking (&optional arg)
  "Toggle whether `tex-magic-space' checks `tex-magic-space-tests'.
With prefix argument ARG, activate checking if ARG is positive,
otherwise deactivate it.

Sets `tex-magic-space-do-checking'."
  (interactive "P")
  (setq tex-magic-space-do-checking
	(if (null arg) (not tex-magic-space-do-checking)
	  (> (prefix-numeric-value arg) 0)))
  (if tex-magic-space-mode
      (force-mode-line-update)
    (message "Checking tests for tex-magic-space %sctivated."
	     (if tex-magic-space-do-checking "a" "dea"))))


;;; 'Registering' the tex-magic-space-mode; based on code from reftex.el
(if (fboundp 'add-minor-mode)
    (progn
      ;; the following doesn't seem to work like I'd like it to 
      (put 'tex-magic-space-mode :included '(memq major-mode '(latex-mode
							       tex-mode)))
      (put 'tex-magic-space-mode :menu-tag "TeX Magic Space")
      (add-minor-mode 'tex-magic-space-mode
		      (list " ~" '(tex-magic-space-do-checking ":Chk"))
		      tex-magic-space-mode-map))
  (unless (assq 'tex-magic-space-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(tex-magic-space-mode (" ~" (tex-magic-space-do-checking ":Chk")))
		minor-mode-alist)))
  (unless (assq 'tex-magic-space-mode-map minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'tex-magic-space-mode tex-magic-space-mode-map)
		minor-mode-map-alist))))


;;;; ======================================================================
;;;; Inicjalization

;;; Initialization by Jakub Narebski <jnareb@fuw.edu.pl>
;;; and Adam Przepiorkowski <adamp_at@at_ipipan.waw.pl>

;; `C-c SPC' set via `mode-specific-map' keymap
(define-key mode-specific-map " " 'tex-magic-space-mode)
;(define-key mode-specific-map "@" 'tex-magic-space-toggle-checking)

;; Turn on TeX Magic Space mode for known (La)TeX modes
(defmacro tex-magic-space-mode-add-to-hook (hook)
  "Add `turn-on-tex-magic-space-mode' to HOOK."
  `(add-hook ,hook 'turn-on-tex-magic-space-mode))

(defmacro tex-magic-space-mode-initialize (hooks)
  "Add `turn-on-tex-magic-space-mode' to each of HOOKS."
  `(dolist (hook ,hooks)
     (tex-magic-space-mode-add-to-hook hook)))

(defvar tex-magic-space-mode-hooks-list
  '(TeX-mode-hook LaTeX-mode-hook 	; for AUCTeX
    tex-mode-hook			; for tex-mode
    reftex-mode-hook			; for RefTeX minor mode
    bibtex-mode-hook)			; for BibTeX
  "List of hooks to which add turning on TeX Magic Space minor mode.")

(defun turn-on-tex-magic-space-in-tex-modes ()
  "Turn on TeX Magic Space mode automatically in TeX modes.
Adds `turn-on-tex-magic-space-mode' to the hooks listed in the
variable `tex-magic-space-mode-hooks-list'."
  (tex-magic-space-mode-initialize tex-magic-space-mode-hooks-list))



;;;; ======================================================================
;;;; Announce

(provide 'sierotki)

;;; sierotki.el ends here.
