;;; sierotki.el --- Introduce tildes after single-letter words

;; Copyright (C) 2002  Michal Jankowski, Jakub Narebski

;; Author: Michal Jankowski <michalj@fuw.edu.pl>
;;	Jakub Narebski <jnareb@fuw.edu.pl>
;;	Adam P. <adamp_at@at_ipipan.waw.pl>
;; Maintainer: Jakub Narebski <jnareb@fuw.edu.pl>
;; Version: 1.3
;; Keywords: tex, wp
;; Created: 03-11-1999

;; $Id$

;;; Commentary:

;;; Code:


;;;; ======================================================================
;;;; Usuwanie sierotek w istniej±cym dokumencie, interaktywne.

;;; Hard spaces by Ryszard Kubiak <rysiek@ipipan.gda.pl>
;;; Modified by Jakub Narêbski <jnareb@fuw.edu.pl>

;; Introduce (interactively) tilde characters (\TeX's hard-space ~)
;; after one-letter Polish articles in the entire buffer.
;; Poni¿sza zmienna definiuje wyra¿enie regularne u¿ywane w `tex-hard-spaces'
(defvar tex-hard-spaces-regexp "\\<\\([aeiouwzAEIOUWZ]\\)\\s +"
  "*Regular expression which detects single [aeiouwz] for `tex-hard-spaces'.
The part of regular expression which matches string to be saved 
should be in parentheses, so the replace part \\\\1~ will work.

Used as first argument to `query-replace-regexp'.")

;; Zwyk³e `query-replace-regexp', czyli C-M-% dla odpowiednich wyra¿eñ regularnych
(defun tex-hard-spaces ()
  "Replaces space after single-letter word with '~', 
the TeX non-breakable space"
 (interactive)
 (query-replace-regexp tex-hard-spaces-regexp
                       "\\1~"))


;;;; ======================================================================
;;;; Zapobieganie powstawaniu sierotek 'w locie'

(defvar tex-magic-space-regexp "\\<[aeiouwzAEIOUWZ]\\'"
  "*Regular expression which detects single [aeiouwz] for `tex-magic-space'.
This regular expression should end with \\\\' to match against \"point\",
and begin with something matching against word boundary.

Used in comparing with part of buffer before point.")

;;; Magic space by Michal Jankowski <michalj@fuw.edu.pl>
(defun tex-magic-space () 
  "Magic-space - inserts non-breakable space after a single-letter word." 
  (interactive)
  (if (string-match 
       tex-magic-space-regexp
       (buffer-substring (max (point-min) (- (point) 2)) (point)))
      (insert "~")
    (insert " ")
    (and auto-fill-function
	 (funcall auto-fill-function))))

;;; ----------------------------------------------------------------------
;;; Toggle magic space by Jakub Narêbski <jnareb@fuw.edu.pl>, 

;; Przypisuje/wy³±cza przypisanie tex-magic-space do spacji,
;; (przydatne przy pisaniu matematyki), [tylko dla trybów LaTeX-owych]
(defun tex-toggle-magic-space ()
  "Toggles TeX magic space, which inserts non-breakable space after a
single-letter word"
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
