;;; sierotki.el --- Introduce tildes after single-letter words

;; Copyright (C) 2002  Michal Jankowski, Jakub Narebski

;; Author: Michal Jankowski <michalj@fuw.edu.pl>
;;	Jakub Narebski <jnareb@fuw.edu.pl>
;;	Adam P. <adamp_at@at_ipipan.waw.pl>
;; Maintainer: Jakub Narebski <jnareb@fuw.edu.pl>
;; Version: 2.2
;; Keywords: tex, wp
;; Created: 03-11-1999

;; $Id$

;; Copyright (C) 2002  Michal Jankowski, Jakub Narebski
     
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

;;; Commentary:

;;; Put the following line in your .emacs file

;; (require 'sierotki)

;;; This file defines only two variables and three functions. 
;;; Heavily documented by Jakub Nar�bski.


;;; Commentary[pl]:

;;; Umie�� nast�puj�c� linijk� w swoim pliku .emacs

;; (require 'sierotki)

;;; Ten plik/pakiet definiuje tylko dwie zmienne i trzy funkcje.
;;; Dokumentacja i komentarze: Jakub Nar�bski.


;;; Notes[pl]:

;;; TO DO: Zrobi� wolniejsz� wersj� `tex-magic-space', kt�ra b�dzie
;;;        np. sprawdza�a czy jeste�my w trybie matematycznym tak jak
;;;        `TeX-insert-dollar' za pomoc� `texmathp' z AUCTeX-a.
;;;
;;; IDEA: `texmathp' jest dost�pne tylko w AUC TeX-u (standardowe tex-mode.el
;;; dost�pne z Emacsem nie zawiera AFAIK podobnego makra).  Jest ono zdefiniowane
;;; w texmathp, autoloaded.  Chcemy by `tex-magic-space' dzia�a�o zar�wno w
;;; standardowym `tex-mode'/`latex-mode', jak i w AUC TeX-owym
;;; `TeX-mode'/`LaTeX-mode', przy czym w tym drugim chcemy mie� mo�liwo��
;;; skorzystania z `texmathp' , w tym pierwszym ewentualnie sprawdza� (podobnie
;;; jak w `comment-beginning') czy u�ywamy `tex-math-face' co si� sprawdza
;;; przynajmniej w $$ ... $$ i $ ... $.  Mo�na by to zrobi� u�ywaj�c porady
;;; (advice) i dodaj�c aktywacj�/deaktywacj� tej porady do
;;; `TeX-mode-hook'/`LaTeX-mode-hook' (w ka�dym razie pr�buj�c j� aktywowa� przy
;;; w��czaniu odpowiedniego trybu, deaktywuj�c przy jego wy��czaniu; np. za
;;; pomoc� `eval-after-load').  Funkcja kt�ra by aktywowa�a/deaktywowa�a porad�
;;; powinna u�ywa� `featurep' by sprawdzi�, czy zosta� za�adowany AUCTeX
;;; (tex-site, latex, tex; texmathp.el nie dostarcza �adnej cechy (feature)); lub
;;; `require' z parametrem NOERROR, sprawdzaj�c czy uda�o si� za�adowa� plik.
;;; Ewentualnie mo�na by u�ywa� `texmathp' (kt�re jest automatycznie �adowane
;;; je�li AUCTeX jest zainstalowany) wewn�trz "pu�apki" `condition-case' lub
;;; `unwind-protect', z czego oba rozwi�zania umo�liwiaj� skorzystanie ze
;;; sprawdzania `tex-math-face' je�li `texmathp' jest niedost�pne.  
;;;
;;; PRZYK�AD (testowy):
;;; (defun test-math ()
;;;   (interactive)
;;;   (condition-case err
;;;       (and (texmathp)
;;; 	   (message "Why: %s" (princ texmathp-why))
;;; 	   (message "Face: %s" (princ (get-text-property (point) 'face))))
;;;     ;; This is the handler; it is not a form
;;;     (error (princ (format "The error was: %s\n" err))
;;; 	   nil)))
;;;
;;; UWAGA: R�ne pliki r�nie definiuj� font (face) dla trybu matematycznego:
;;; * AUCTeX: font-latex.el:  font-latex-math-face (LaTeX math expressions)
;;; * AUCTeX: hilit-LaTeX.el: w�asne funkcje, u�ywa hilit19
;;; * AUCTeX: tex-font.el:    tex-math-face (TeX math expressions)
;;; * Emacs:  tex-mode.el:    tex-math-face (TeX math expressions)
;;; ale mo�na u�ywa� `tex-math-face'.
;;;
;;; Mo�na tak�e sprawdza� czy jeste�my w komentarzu u�ywaj�c kodu jak w
;;; `comment-beginning', t.j. sprawdzaj�c czy u�ywamy `font-lock-comment-face' i
;;; ewentualnie szukaj�c znaku komentarza `%' w bie��cej linii na lewo od
;;; bie��cej pozycji (`point').


;;; Ponadto dokumentacja po angielsku (zw�aszcza docstrings) wymaga poprawienia.
;;;`tex-toggle-magic-space' dzia�a w dowolnym trybie (patrz komentarz).


;;; Code:


;;;; ======================================================================
;;;; Usuwanie sierotek w istniej�cym dokumencie, interaktywne.

;;; Hard spaces by Ryszard Kubiak <rysiek@ipipan.gda.pl>
;;; Modified by Jakub Nar�bski <jnareb@fuw.edu.pl>

;; Zast�puje znaki odst�pu przez znaki tyldy `~', czyli TeX-ow� nie�amliw�
;; spacj�, po jednoliterowych [polskich] sp�jnikach w ca�ym buforze.
;; Poni�sza zmienna definiuje wyra�enie regularne u�ywane w `tex-hard-spaces'
(defvar tex-hard-spaces-regexp "\\<\\([aeiouwzAEIOUWZ]\\)\\s +"
  ;; Pierwsza linia dokumentacji jest zbyt d�uga: 
  ;; nie powinna przekracza� 67 znak�w (jest 76 - 3 = 73)
  "*Regular expression which detects single [aeiouwz] for `tex-hard-spaces'.
The part of regular expression which matches string to be saved 
should be in parentheses, so the replace part \\\\1~ will work.

Used as first argument to `query-replace-regexp'.")

;; Zwyk�e `query-replace-regexp', czyli C-M-% dla odpowiedniego
;; wyra�enia regularnego, zapisanego w `tex-hard-spaces-regexp'
(defun tex-hard-spaces ()
  ;; Pierwsza linia dokumentacji 
  ;; nie powinna przekracza� 67 znak�w (jest 68 - 3 = 65)
  "Replaces whitespace characters after single-letter word with `~'.
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

;;; Magic space by Michal Jankowski <michalj@fuw.edu.pl>
;;; Modified by Jakub Nar�bski <jnareb@fuw.edu.pl>

;; UWAGA: [czasami] polskie literki s� traktowane jako koniec s�owa dla 8bit
;;        tzn. przy u�yciu `standard-display-european' do ich wprowadzania.
;;        B�d� pr�bowac znale�� dok�adne warunki wyst�pienia b�edu.
;; TO DO: U�y� `defcustom' 
;; TO DO: Doda� tex-magic-space-regexp-len zamiast 2 (np. dowi�zywanie 'tys.')
(defvar tex-magic-space-regexp "\\<[aeiouwzAEIOUWZ]\\'"
  ;; Pierwsza linia dokumentacji jest zbyt d�uga: 
  ;; nie powinna przekracza� 67 znak�w (jest 76 - 3 = 73)
  "*Regular expression which detects single [aeiouwz] for `tex-magic-space'.
`tex-magic-space' inserts `~' if this expression matches two characters before point, 
otherwise it inserts the key it is bound to (\\[tex-magic-space]), usually SPC.

This regular expression should end with [aeiouwzAEIOUWZ]\\\\' to match possible
single letter conjunction against the letter directly before the point.
The part before [aeiouwzAEIOUWZ] should match word beginning/boundary.

ATTENTION: sometimes in unibyte mode the non US-ASCII letters are considered
word boundary, even when they are word constituents.")

(defun tex-magic-space (prefix) 
  ;; Pierwsza linia dokumentacji jest zbyt d�uga: 
  ;; nie powinna przekracza� 67 znak�w (jest 72 - 3 = 69)
  "Magic-space - inserts non-breakable space after a single-letter word. 
Uses `tex-magic-space-regexp' for single-letter words detection.

Works well with auto filling unless `~' is in the table `auto-fill-chars',
in which case `~' is inserted but might be followed by line break.
Works with abbrev expansion with the following exceptions:
 - doesn't do abbrev expansion if abbrev is single letter word 
   and `~' is word constituent (according to current syntax table)
 - abbrevs with expansion ending with single-letter word won't have
   the SPC following single-letter word substituted with `~'

Bind it to space using \\[local-set-key] SPC tex-magic-space 
or `tex-toggle-magic-space' (\\[tex-toggle-magic-space]).

See also: `tex-hard-spaces'"
  (interactive "p")	                ; Prefix arg jako liczba.  Nie robi I/O.
  (when (string-match 
	 tex-magic-space-regexp	        ; wyra�enie rozpoznaj�ce samotne sp�jniki
	 (buffer-substring (max (point-min) (- (point) 2)) (point)))
    (setq last-command-char ?~))	; wstawiamy `~' zamiast SPC
  (self-insert-command prefix))	        ; daje obs�ug� auto-fill, abbrev, blinkin-paren


;;; ----------------------------------------------------------------------
;;; Toggle magic space by Jakub Nar�bski <jnareb@fuw.edu.pl>, 
;;; modifications based on code by Adam P. <adamp_at@at_ipipan.waw.pl>

;; Przypisuje/wy��cza przypisanie tex-magic-space do spacji,
;; (przydatne przy pisaniu matematyki), [tylko dla tryb�w LaTeX-owych]
;; TO DO: Zrobi� z tego minor mode.
(defun tex-toggle-magic-space (&optional arg)
  "Toggle whether SPC is bound to `tex-magic-space'.
With prefix argument ARG, bind SPC to `tex-magic-space' if ARG is positive, 
otherwise bind SPC to `self-insert-command'.

It can be used to toggle temporarily `tex-magic-space' off when writing
equations (with e.g. `i' as index), then turn it on in main text.

Uses local keymap i.e. major mode keymap, so it currently works with
any mode, not only with LaTeX modes (there are several of them and
they do not use one common keymap)."
  (interactive "P")	                ; Prefix arg w postaci surowej.  Nie robi I/O.
  (progn			        ; u�ywane tylko by wypisa� komunikat
    (cond
     ((null arg)			; Brak prefiksu
      (if (local-key-binding " " 'tex-magic-space)
	  (local-unset-key " ")     
	(local-set-key " " 'tex-magic-space)))
     ((> (prefix-numeric-value arg) 0) ; Dodatni argument
      (local-set-key " " 'tex-magic-space))
     (t				; wpp (niedodatni argument)
      (local-unset-key " ")))
    ;; koniec cond; opisanie wyniku
    (describe-key-briefly " ")))


;;;; ---------------------------------------------------------------------
;;;; Inicjalizacja dla zapobiegania powstawaniu sierotek 'w locie'

;;; Initialization, by Jakub Nar�bski <jnareb@fuw.edu.pl> 
;;; and Adam P. <adamp_at@at_ipipan.waw.pl>

;; C-c SPC toggles magic space:
;; `mode-specific-map' is keymap for characters following C-c
;; Sequences consisting of `C-c' followed by any punctuation character 
;; other than `{', `}', `<', `>', `:', `;' are allocated for minor modes.
(define-key  mode-specific-map " " 'tex-toggle-magic-space) ; C-c SPC


;;; Initialize SPC to `tex-magic-space' using eval-after-load
;; For AUC TeX
(eval-after-load "tex"      '(define-key TeX-mode-map    " " 'tex-magic-space))
(eval-after-load "latex"    '(define-key LaTeX-mode-map  " " 'tex-magic-space))
;; For tex-mode included in Emacs
(eval-after-load "tex-mode" '(define-key tex-mode-map    " " 'tex-magic-space))
;; For RefTeX
;; NOTE: RefTeX to minor mode, keymap zas�ania lokalny keymap
;; (np. LaTeX-mode-keymap) i aby `tex-toggle-magic-space' dzia�a�o trzeba by
;; jawnie u�ywa� `reftex-mode-map', a nie wiadomo czy RefTeX zosta� za�adowany
;(eval-after-load "reftex"   '(define-key reftex-mode-map " " 'tex-magic-space))

;; Aby mo�na by�o �adowa� ten plik zar�wno za pomoc� 
;; (load "sierotki")
;; jak i
;; (requires 'sierotki)
(provide 'sierotki)

;;; sierotki.el ends here
