;;; sierotki.el --- Introduce tildes after single-letter words

;; Copyright (C) 2002  Michal Jankowski, Jakub Narebski

;; Author: Michal Jankowski <michalj@fuw.edu.pl>
;;	Jakub Narebski <jnareb@fuw.edu.pl>
;;	Adam P. <adamp_at@at_ipipan.waw.pl>
;; Maintainer: Jakub Narebski <jnareb@fuw.edu.pl>
;; Version: 2.0rc1
;; Keywords: tex, wp
;; Created: 03-11-1999

;; $Id$

;;; Commentary:

;;; Code:


;;;; ======================================================================
;;;; Usuwanie sierotek w istniej±cym dokumencie, interaktywne.

;;; Hard spaces by Ryszard Kubiak <rysiek@ipipan.gda.pl>
;;; Modified by Jakub Narêbski <jnareb@fuw.edu.pl>

;; Zastêpuje znaki odstêpu przez znaki tyldy `~', czyli TeX-ow± nie³amliw±
;; spacjê, po jednoliterowych [polskich] spójnikach w ca³ym buforze.
;; Poni¿sza zmienna definiuje wyra¿enie regularne u¿ywane w `tex-hard-spaces'
(defvar tex-hard-spaces-regexp "\\<\\([aeiouwzAEIOUWZ]\\)\\s +"
  "*Regular expression which detects single [aeiouwz] for `tex-hard-spaces'.
The part of regular expression which matches string to be saved 
should be in parentheses, so the replace part \\\\1~ will work.

Used as first argument to `query-replace-regexp'.")

;; Zwyk³e `query-replace-regexp', czyli C-M-% dla odpowiednich wyra¿eñ regularnych
(defun tex-hard-spaces ()
  "Replaces spaces after single-letter word with '~', the TeX non-breakable space.
Replaces whitespace characters following single-letter conjunctions by `~',
the TeX non-breakable space in whole buffer, interactively.
Uses `tex-hard-spaces-regexp' for single-letter conjunctions detection.

It can be used to bind single-letter conjunction to the word following it in
the existing text, using `~' (the TeX non-breakable space), so there are no
single-letter conjunctions at the end of the line (known as 'orphans'). 
For on-the-fly 'orphans' elimination bind SPC to `tex-magic-space' 
using \\[tex-toggle-magic-space].

It is implemented using `query-replace-regexp'."
 (interactive)
 (query-replace-regexp tex-hard-spaces-regexp
                       "\\1~"))


;;;; ======================================================================
;;;; Zapobieganie powstawaniu sierotek 'w locie'

;; UWAGA: [czasami] polskie literki s± traktowane jako koniec s³owa dla 8bit
;;        tzn. przy u¿yciu `standard-display-european' do ich wprowadzania.
;;        Bêdê próbowac znale¼æ dok³adne warunki wyst±pienia b³edu.
;; TO DO: U¿yæ `defcustom'
(defvar tex-magic-space-regexp "\\<[aeiouwzAEIOUWZ]\\'"
 "*Regular expression which detects single [aeiouwz] for `tex-magic-space'.
`tex-magic-space' inserts `~' if this expression matches two characters before point, 
otherwise it inserts the key it is bound to (\\[tex-magic-space]), usually SPC.

This regular expression should end with [aeiouwzAEIOUWZ]\\\\' to match possible
single letter conjunction against the letter directly before the point.
The part before [aeiouwzAEIOUWZ] should match word beginning/boundary.

ATTENTION: sometimes in unibyte mode the non US-ASCII letters are considered
word boundary, even when they are word constituents.")

;;; Magic space by Michal Jankowski <michalj@fuw.edu.pl>
;;; Modified by Jakub Narêbski <jnareb@fuw.edu.pl>
(defun tex-magic-space (arg) 
  "Magic-space - inserts non-breakable space after a single-letter word. 
Uses `tex-magic-space-regexp' for single-letter words detection.

Bind it to space using \\[local-set-key] SPC tex-magic-space 
or `tex-toggle-magic-space' (\\[tex-toggle-magic-space]).

See also: `tex-hard-spaces'"
  (interactive "p")
  (when (string-match 
	 tex-magic-space-regexp
       (buffer-substring (max (point-min) (- (point) 2)) (point)))
    (setq last-command-char ?~))
  (self-insert-command arg))

;;; ----------------------------------------------------------------------
;;; Toggle magic space by Jakub Narêbski <jnareb@fuw.edu.pl>, 

;; Przypisuje/wy³±cza przypisanie tex-magic-space do spacji,
;; (przydatne przy pisaniu matematyki), [tylko dla trybów LaTeX-owych]
(defun tex-toggle-magic-space ()
  "Toggle whether SPC is bound to `tex-magic-space'.

It can be used to toggle temporarily `tex-magic-space' off when writing
equations (with e.g. `i' as index), then turn it on in main text.

Uses `current-local-map' or `current-local-map', so it currently works with 
any mode, not only with LaTeX modes (there are several of them and they do not
use one common keymap)."
  (interactive)
  (let 
    ((map (or (current-local-map)
              (current-global-map))))
    (progn
      (if (equal (lookup-key map " ") 'tex-magic-space)
          (progn
            (define-key map " " nil)
            (local-unset-key " ")) ; to be sure
        (define-key map " " 'tex-magic-space))
    (describe-key-briefly " "))))


;;; ---------------------------------------------------------------------
;;; Inicjalizacja dla zapobiegania powstawaniu sierotek 'w locie'

;;; Initialization, by Jakub Narêbski <jnareb@fuw.edu.pl> 
;;; and Adam P. <adamp_at@at_ipipan.waw.pl>

;; C-c SPC toggles magic space:
;; `mode-specific-map' is keymap for characters following C-c
;; Sequences consisting of `C-c' followed by any punctuation character 
;; other than `{', `}', `<', `>', `:', `;' are allocated for minor modes.
;; by Adam P. <adamp_at@at_ipipan.waw.pl>
(define-key mode-specific-map " " 'tex-toggle-magic-space) ; C-c SPC

;;; Initialize SPC to `tex-magic-space' using eval-after-load
;; For AUC TeX
(eval-after-load "tex"      '(define-key TeX-mode-map    " " 'tex-magic-space))
(eval-after-load "latex"    '(define-key LaTeX-mode-map  " " 'tex-magic-space))
;; For tex-mode as included in Emacs
(eval-after-load "tex-mode" '(define-key tex-mode-map    " " 'tex-magic-space))
;; For RefTeX
(eval-after-load "reftex"   '(define-key reftex-mode-map " " 'tex-magic-space))

;;; sierotki.el ends here
