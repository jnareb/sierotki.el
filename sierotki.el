;;; sierotki.el --- Introduce tildes after single-letter words

;; Copyright (C) 2002  Micha� Jankowski, Jakub Nar�bski

;; Author: Ryszard Kubiak        <rysiek@ipipan.gda.pl>
;;		Micha� Jankowski <michalj@fuw.edu.pl>
;;		Jakub Nar�bski   <jnareb@fuw.edu.pl>
;; Maintainer: 	Jakub Narebski <jnareb@fuw.edu.pl>
;; Version: 	2.3.2
;; RCS version:	$Revision$
;; Date: 	$Date$
;; Keywords: 	tex, wp
;; Created: 	03-11-1999

;; $Id$

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

;;}}}

;;; Commentary:

;;; Umie�� nast�puj�c� linijk� w swoim pliku .emacs

;; (require 'sierotki)

;;; Ten pakiet s�u�y do dowi�zywania zdefiniowanych wyraz�w (domy�lnie
;;; jednoliterowych sp�jnik�w) do nast�puj�cych po nich s��w za pomoc� znaku
;;; `~' (tyldy), nie�amliwej spacji TeX-owej.  S�u�y to temu, aby w
;;; dokumentach TeX-owych unikn�� jednoliterowych sp�jnik�w na ko�cach linii,
;;; co jest wymagane przez polskie (i czeskie) regu�y typograficzne.

;;; Pakiet ten dostarcza dwu funkcjonalno�ci.  Pierwsz� z nich jest
;;; sprawdzenie (istniej�cego) tekstu i zasugerowanie dodania brakuj�cych
;;; tyld.  Jest ona implementowana przez komend� `tex-hard-spaces', za pomoc�
;;; `query-replace-regexp'.  T� sam� (a nawet rozszerzon�) funkcjonalno��
;;; znale�� mo�na w pakiecie `tildify' (UWAGA: domy�lne ustawienia w tym
;;; pakiecie s� dostosowane do j�zyka czeskiego).

;;; Drug� z funkcjonalno�ci jest automatyczne wpisywanie tyld po
;;; jednoliterowych sp�jnikach podczas pisania tekstu (w locie).  Jest
;;; ona implementowana przez komend� `tex-magic-space', kt�r� nale�y
;;; podpi�� do spacji.  Do aktywowania tej funkcjonalno�ci mo�na u�y�
;;; `tex-toggle-magic-space', albo (co jest bezpieczniejsze)
;;; `tex-magic-space-mode'.  Tryb (minor mode) TeX Magic Space mo�na
;;; aktualnie w��czy� z modeline dla tryb�w g��wnych (major mode)
;;; `latex-mode' lub `tex-mode'; jest on oznaczany za pomoc� " ~".
;;;
;;; Funkcjonalno�� ta jest automatycznie w��czana w trybach TeX-owych
;;; za pomoc� `eval-after-load'.

;;; Dokumentacja i komentarze: Jakub Nar�bski.


;;; Notes:

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
;;; `comment-beginning', t.j. sprawdzaj�c czy u�ywamy `font-lock-comment-face'
;;; i ewentualnie szukaj�c znaku komentarza `%' w bie��cej linii na lewo od
;;; bie��cej pozycji (`point').


;;; Ponadto dokumentacja po angielsku (zw�aszcza docstrings) wymaga poprawienia.
;;; `tex-toggle-magic-space' dzia�a w dowolnym trybie (patrz komentarz).


;;; History:

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
     ((null arg)			; brak prefiksu
      (if (local-key-binding " " 'tex-magic-space)
	  (local-unset-key " ")
	(local-set-key " " 'tex-magic-space)))
     ((> (prefix-numeric-value arg) 0) ; dodatni prefiks
      (local-set-key " " 'tex-magic-space))
     (t					; wpp (niedodatni prefiks)
      (local-unset-key " ")))
    ;; koniec cond; opisanie wyniku
    (describe-key-briefly " ")))


;;;; ======================================================================
;;;; `tex-magic-space-mode': TeX Magic Space as buffer local minor mode
(defvar tex-magic-space-mode nil
  "Determines if TeX Magic Space mode is active.")
(make-variable-buffer-local 'tex-magic-space-mode)

(defvar tex-magic-space-mode-map (make-sparse-keymap)
  "Keymap for TeX Magic Space mode, containing only entry for SPC.")

(define-key tex-magic-space-mode-map " " 'tex-magic-space)

;;;###autoload
(defun turn-on-tex-magic-space-mode ()
  "Turn on TeX Magic Space mode."
  (tex-magic-space-mode t))

;;;###autoload
(defun tex-magic-space-mode (&optional arg)
  "Toggle TeX Magic Space mode.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\<tex-magic-space-mode-map>
In this (buffer local) mode `\\[tex-magic-space]' runs the command
`tex-magic-space'."
  (interactive "P")
;;;  (setq tex-magic-space-mode 		
;;;	(if (null arg) (not tex-magic-space-mode)
;;;	  (> (prefix-numeric-value arg) 0))))
  (setq tex-magic-space-mode 
	(not (or (and (null arg) tex-magic-space-mode)
		 (<= (prefix-numeric-value arg) 0))))
  (force-mode-line-update))

;;; something like this was found in reftex
(if (or 
     ;; in FSF Emacs `add-minor-mode' is defined in file `easy-mmode'
     (featurep 'easy-mmode)     
     ;; in XEmacs it is defined in `modeline', which doesn't do `provide'
     (fboundp 'add-minor-mode)) 

    ;; In Emacs this is XEmacs compatibility function.
    ;; Use it so that we get the extras i.e. mode-line minor mode menu
    (progn
      (put 'tex-magic-space-mode :included '(memq major-mode '(latex-mode tex-mode)))
      (put 'tex-magic-space-mode :menu-tag "TeX Magic Space")
      ;; IDEA: tooltip, toggle magic space as toggle read only in modeline
      ;; :help-echo property, :local-map for toggle, maybe :display, :*face
      (add-minor-mode 'tex-magic-space-mode " ~" tex-magic-space-mode-map))

  ;; The standard way
  (unless (assq 'tex-magic-space-mode minor-mode-alist)
    (setq minor-mode-alist 
	  (cons '(tex-magic-space-mode " ~") 
		minor-mode-alist)))
  (unless (assq 'tex-magic-space-mode-map minor-mode-map-alist)
    (setq minor-mode-map-alist 
	  (cons (cons 'tex-magic-space-mode tex-magic-space-mode-map) 
		minor-mode-map-alist))))


;;;; ======================================================================
;;;; Inicjalizacja dla zapobiegania powstawaniu sierotek 'w locie'

;;; Initialization by Jakub Nar�bski <jnareb@fuw.edu.pl>
;;; and Adam Przepi�rkowski <adamp_at@at_ipipan.waw.pl>

;; Globally bind TeX Magic Space mode to `C-c SPC'
(define-key mode-specific-map " " 'tex-magic-space-mode) ; C-c SPC

;; Turn on TeX Magic Space for known (La)TeX modes (at loading)
;; For AUC TeX
(eval-after-load "tex"      '(setq tex-magic-space-mode t))
(eval-after-load "latex"    '(setq tex-magic-space-mode t))
;; For tex-mode included in Emacs
(eval-after-load "tex-mode" '(setq tex-magic-space-mode t))
;; For RefTeX
;; NOTE: W tej wersji jest to ca�kowicie bezpieczne
(eval-after-load "reftex"   '(setq tex-magic-space-mode t))


;;;; ======================================================================
;;;; Zako�czenie
;; Aby mo�na by�o �adowa� ten plik zar�wno za pomoc�
;; (load "sierotki") jak i (requires 'sierotki)
(provide 'sierotki)

;;; sierotki.el ends here
