;;; sierotki.el --- Introduce tildes after single-letter words

;; Copyright (C) 2002  Micha� Jankowski, Jakub Nar�bski

;; Author: 	Ryszard Kubiak   <rysiek@ipipan.gda.pl>
;;		Micha� Jankowski <michalj@fuw.edu.pl>
;;		Jakub Nar�bski   <jnareb@fuw.edu.pl>
;; Maintainer: 	Jakub Nar�bski <jnareb@fuw.edu.pl>
;; Version: 	2.4.3-toggle-1
;; RCS version:	$Revision$
;; Date: 	$Date$
;; Keywords: 	TeX, wp, convenience
;; Created: 	03-11-1999
;; URL: 	http://www.fuw.edu.pl/~jnareb/sierotki.el
;;
;; Compatibility:   Emacs21, XEmacs21
;; Incompatibility:

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


;;; Installation:

;; Aby u�y� tego pakietu, umie�� nast�puj�c� linijk� w swoim pliku .emacs
;;
;;    (require 'sierotki)
;;
;; Je�li nie chcesz go �adowa� zanim nie b�dzie potrzebny, mo�esz u�y�
;; �adowania na ��danie, np. dodaj�c do .emacs nast�puj�ce linie
;;
;;    (autoload 'tex-magic-space-mode "sierotki"
;;              "TeX Magic Space minor mode" t)
;;    (define-key mode-specific-map " " 'tex-magic-space-mode)
;;
;; W�wczas po wci�ni�ciu `C-c SPC' zostanie w��czony TeX Magic Space mode
;; i zostanie za�adowana reszta funkcji.  Uwaga: przy u�ywaniu
;; automatycznego �adowania ten tryb _nie_ b�dzie automatycznie w��czany
;; w trybach LaTeX-owych.


;;; Commentary:

;; Ten pakiet s�u�y do dowi�zywania zdefiniowanych wyraz�w (domy�lnie
;; jednoliterowych sp�jnik�w) do nast�puj�cych po nich s��w za pomoc� znaku
;; `~' (tyldy), nie�amliwej spacji TeX-owej.  S�u�y to temu, aby w
;; dokumentach TeX-owych unikn�� jednoliterowych sp�jnik�w na ko�cach linii,
;; co jest wymagane przez polskie (i czeskie) regu�y typograficzne.
;;
;; Pakiet ten dostarcza dwu funkcjonalno�ci.  Pierwsz� z nich jest
;; sprawdzenie (istniej�cego) tekstu i zasugerowanie dodania brakuj�cych
;; tyld.  Jest ona implementowana przez komend� `tex-hard-spaces', za pomoc�
;; `query-replace-regexp'.  T� sam� (a nawet rozszerzon�) funkcjonalno��
;; znale�� mo�na w pakiecie `tildify' (UWAGA: domy�lne ustawienia w tym
;; pakiecie s� dostosowane do j�zyka czeskiego).
;;
;; Drug� z funkcjonalno�ci jest automatyczne wpisywanie tyld po
;; jednoliterowych sp�jnikach podczas pisania tekstu (w locie).  Jest
;; ona implementowana przez komend� `tex-magic-space', kt�r� nale�y
;; podpi�� do spacji.  Do aktywowania tej funkcjonalno�ci mo�na u�y�
;; `tex-toggle-magic-space', albo (co jest bezpieczniejsze)
;; `tex-magic-space-mode'.  Tryb (minor mode) TeX Magic Space mo�na
;; aktualnie w��czy� z modeline dla tryb�w g��wnych (major mode)
;; `latex-mode' lub `tex-mode'; jest on oznaczany za pomoc� " ~".
;; Ewentualne dodatkowe oznaczenia po " ~" informuj�, ze porady s� aktywne
;; i pokazuj� kt�re porady s� w��czone.
;;
;; Funkcjonalno�� ta jest automatycznie w��czana w trybach TeX-owych
;; za pomoc� dodania odpowiednika `turn-on-tex-magic-space-mode' do
;; odpowiednich haczyk�w (zdefiniowanych w zmiennej
;; `tex-magic-space-mode-hooks-list') za pomoc� `add-hook'.

;; Dokumentacja i komentarze: Jakub Nar�bski.


;;; Notes:

;; W��czanie i aktywacja porady `tex-magic-space-texmathp' (ze standardowymi
;; warto�ciami zmiennych dla `texmathp') powoduje oko�o 10-krotne zwolnienie
;; dzia�ania `tex-magic-space' (zmierzono za pomoca pakietu "elp").


;;; To do:

;; TO DO: Ulepszy� wyra�enie regularne b�d� da� do wyboru wersj� prost�
;; (i szybk�) lub skomplikowan� (i mog�c� wi�cej).  By� mo�e wyra�enie
;; regularne powinno by� "sk�adane" z kilku: definiuj�cego granic� s�owa z
;; lewej strony, definiuj�cego jednoliterowe sp�jniki, definiuj�cego inne
;; wyrazy po kt�rych stawiamy nie�amliw� spacj�, definiuj�cego dowi�zanie z
;; prawej strony t.j. co� jak "\\'", definiuj�cego jakie znaki zamieniamy na
;; pojedyncz� nie�amliw� spacj� (granic� s�owa z prawej).
;;
;; TO DO: Osobna porada (domy�lnie wy��czona) sprawdzaj�ca czy jeste�my w
;; komentarzu u�ywaj�c kodu jak w `comment-beginning', t.j. sprawdzaj�c czy
;; u�ywamy `font-lock-comment-face' i ewentualnie szukaj�c znaku komentarza
;; `%' w bie��cej linii na lewo od bie��cej pozycji (`point').
;;
;; TO DO: Zgadywanie, czy nale�y w��czy� TeX Magic Space mode (czy w inny
;; spos�b uaktywni� magiczn� spacj�) na podstawie nag��wka pliku TeX-owego.
;;
;;
;; Ponadto dokumentacja po angielsku (zw�aszcza docstrings) wymaga poprawienia.
;; `tex-toggle-magic-space' dzia�a w dowolnym trybie (patrz komentarz).
;;
;; Zawarto�� "History:" nie jest kompletna.

;;; History:

;; Kod `tex-hard-spaces' pojawi� si� po raz pierwszy w:
;;
;; From: rysiek@IPIPAN.GDA.PL (Ryszard Kubiak)
;; Newsgroups: pl.comp.dtp.tex.gust
;; Subject: Re: tylda do samotnych
;; Date: 25 Oct 1999 21:12:54 GMT
;;
;; Wpisywanie tyld "w locie", tzn `tex-magic-space' pojawi�o si� w:
;;
;; From: Michal Jankowski <michalj@fuw.edu.pl>
;; Newsgroups: pl.comp.dtp.tex
;; Subject: Dowiazywanie samotnich literek do nastepnego slowa.
;; Date: 03 Nov 1999 12:45:22 +0100
;;
;; Nast�pnie wyra�enia regularne w obu funkcjach by�y sukcesywnie
;; poprawiane.  W wyniku do�wiadcze� z u�ywania `tex-magic-space' przy
;; pisaniu tekst�w z du�� ilo�ci� matematyki zosta�o napisane
;; `tex-toggle-magic-space'.  Nast�pnie zosta� zg�oszony b��d w wyra�eniu
;; regularnym w `tex-magic-space', a w wyniku dyskusji powsta�a obecna
;; wersja `tex-magic-space', u�ywaj�ca zmiennej `last-command-char'
;; i funkcji `self-insert-command'
;;
;; From: Michal Jankowski <Michal.Jankowski@fuw.edu.pl>
;; Subject: Re: Test sierotek
;; Date: 30 Oct 2001 13:02:16 +0100
;;
;; W wyniku por�wnania z inn� implementacj� magicznej spacji (`spacja')
;; z artyku�u "GNU Emacs Lisp" rzyjontka na debian.otwarte.pl
;; http://debian.otwarte.pl/article.php?aid=39
;; (w szczeg�lno�ci innego jej zachowania) powsta�o pytanie o to, jakie
;; w�asno�ci powinno mie� `tex-magic-space'
;;
;; From: "Jakub Nar�bski" <jnareb@fuw.edu.pl>
;; Subject: RFC: sierotki.el
;; Newsgroups: pl.comp.dtp.tex
;; Date: 14 Nov 2002 14:13:26 GMT
;;
;; Dyskusja trwa...


;;; Change Log:

;; Version 2.3 (RCS revision 1.12):
;; * Pojawi� si� TeX Magic Space minor mode (przypisany do `C-c SPC').
;; Version 2.4 (RCS revision 1.22):
;; * Dodane porady i polecenie do ich w��czana (przypisane do `C-c @'), aby
;;   `tex-magic-space' pozostawa�a nieaktywna tam gdzie nie trzeba (np.
;;   w trybie matematycznym wykrywanym za pomoc� `texmathp').

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
  "Replace whitespace characters after single-letter word with `~'.
Replaces whitespace characters following single-letter conjunctions by `~',
the TeX non-breakable space in whole buffer, interactively.
Uses `tex-hard-spaces-regexp' for single-letter conjunctions detection.

It can be used to bind single-letter conjunction to the word following it in
the existing text, using `~' (the TeX non-breakable space), so there are no
single-letter conjunctions at the end of the line (known as 'orphans').

For on-the-fly 'tildification' bind SPC to `tex-magic-space' using
\\[tex-toggle-magic-space], or turn on TeX Magic Space minor mode using
command \\[tex-magic-space-mode].

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
`tex-magic-space' inserts `~' if this expression matches two characters before
point, otherwise it inserts the key it is bound to (\\[tex-magic-space]),
usually SPC.

This regular expression should end with [aeiouwzAEIOUWZ]\\\\' to match possible
single letter conjunction against the letter directly before the point.  The
part before [aeiouwzAEIOUWZ] should match word beginning/boundary.

ATTENTION: sometimes in unibyte mode the non US-ASCII letters are considered
word boundary, even when they are word constituents.")

(defun tex-magic-space (&optional prefix)
  ;; Pierwsza linia dokumentacji jest zbyt d�uga:
  ;; nie powinna przekracza� 67 znak�w (jest 72 - 3 = 69)
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

Bind it to space using `tex-toggle-magic-space' (\\[tex-toggle-magic-space])
or turn on TeX Magic Space minor mode using command `tex-magic-space-mode'
\(\\[tex-magic-space-mode]).

See also: `tex-hard-spaces'"
  (interactive "p")	               ; Prefix arg jako liczba.  Nie robi I/O.
  (when (string-match
	 tex-magic-space-regexp	       ; wyra�enie rozpoznaj�ce samotne sp�jniki
	 (buffer-substring (max (point-min) (- (point) 2)) (point)))
    (setq last-command-char ?~))       ; wstawiamy `~' zamiast SPC
  (self-insert-command (or prefix 1))) ; daje obs�ug� auto-fill, abbrev, blinkin-paren


;;; ----------------------------------------------------------------------
;;; "Porady" (advices) dla `tex-magic-space'

;; TO DO: Doda� `tex-magic-space-checking-why' (a la `texmathp-why'), kt�re
;;        b�dzie podawa�o dlaczego magiczna spacja jest nieaktywna.
;; IDEE:
;; a. `texmathp', udost�pniane (enable) po za�adowaniu "texmathp"
;; b. a la `texmathp' lub `LaTeX-modify-environment' u�ywane przez
;;    `LaTeX-environment', sprawdzanie czy jeste�my wewn�trz jednego z
;;    otocze� lub pseudootocze� (\verb!...!) zdefiniowanych przez
;;    u�ytkownika, a la `tildify-ignored-environments-alist'; je�li u�ywamy
;;    AUCTeX-a to mogliby�my u�y� polece� `LaTeX-find-matching-begin'
;;    i `LaTeX-find-matching-end', za� do polece� czego� jak w `texmathp',
;;    u�ywaj�c `forward-list' lub `up-list', `forward-sexp' (zawiera tak�e
;;    s�owa, ograniczone �a�cuchy itp.) z odpowiedni� syntax table.
;; c. sprawdzanie czy font (face) nale�y do okre�lonej listy
;; d. zdefiniowana przez u�ytkownika FORM (np. '(and FORM FORM))
;;
;; Ad c. `memq' (u�ywa `eq') i `member' z cl (u�ywa `equal'); je�li w�asno��
;; (property) jest list� nale�y przeiterowa� po jej elementach (let ((idx
;; list)) (while idx ... (setq idx (cdr idx)))) ew. `dolist', lub u�y�
;; `intersection' z pakietu CL (Common Lisp).  Na razie jest to zrobione
;; tak, aby dzia�a�o.


;; IDEE: `tex-magic-space-texmathp'
;; * mo�liwo�ci sprawdzania, czy `texmathp' jest dost�pne:
;;   - sprawdzenie za pomoc� `fboundp' czy symbol `texmathp' ma niepust�
;;     "function cell", tzn. (zazwyczaj) czy funkcja `texmathp' jest
;;     zdefiniowana.
;;   - sprawdzenie za pomoc� `featurep' czy jest dost�pna cecha (feature)
;;     'texmathp, tzn. czy plik "texmathp" zosta� za�adowany (a wi�c i
;;     zadeklarowana w nim funkcja `texmathp' jest dost�pna)
;;   - u�ycie pu�apki `condition-case' lub `unwind-protect', sprawdzaj�c czy
;;     wyst�pi� b��d `void-function' albo jakikolwiek b��d; przyk�ad u�ycia
;;     `condition-case' poni�ej (`unwind-protect' mo�na zrobi� automatycznie
;;     za pomoc� w�asno�ci "protected" przy definiowaniu porady)
;;
;;     (defun test-math ()
;;       (interactive)
;;       (condition-case err
;;           (and (texmathp)
;;      	   (message "Why: %s" (princ texmathp-why))
;;      	   (message "Face: %s" (princ (get-text-property (point) 'face))))
;;         ;; This is the handler; it is not a form
;;         (error (princ (format "The error was: %s\n" err))
;;      	   nil)))
;;
;;   - zapewnienie, �e w chwili uruchomienia porady `texmathp' jest dost�pne
;;     lub zostanie za�adowane na ��danie (autoload), oraz �e porada jest
;;     wy��czona (disabled) gdy `texmathp' jest niedost�pna i jest
;;     automatycznie w��czana (enabled) przy �adowaniu "texmathp" (jako �e
;;     "texmathp" nie udost�pnia �adnych hak�w u�ywamy `eval-after-load').
;;
;; * kompilacja porady `tex-magic-space-texmathp' jest utrudniona przez to,
;;   �e w czasie kompilacji powinna by� dost�pna definicja `texmathp'; mamy
;;   par� mo�liwo�ci
;;   - zignorowa� ten fakt, godz�c si� z tym, �e w czasie kompilacji
;;     `texmathp' mo�e nie by� dost�pne
;;   - za��da�, by bezwarunkowo w czasie kompilacji by�o �adowane
;;     "texmathp", zg�aszaj�c b��d gdy nie jest dost�pne
;;   - �adowa� "texmathp" w czasie kompilacji, ignoruj�c ewentualn� jego
;;     niedost�pno��
;;   - �adowa� "texmathp" w czasie kompilacji, a je�li jest niedost�pne to
;;     nie definiowa� porady `texm-magic-space-texmathp' (np. za pomoc�
;;     wykrywania b��d�w za pomoc� `condition-case' lub ignorowania
;;     niedost�pno�ci i sprawdzania czy "texmathp" zosta�o za�adowane za
;;     pomoc� (featurep 'texmathp))
(eval-when-compile (require 'texmathp))

(defadvice tex-magic-space
  (around tex-magic-space-texmathp (&optional prefix) preactivate)
  "Inactive in math mode as defined by `texmathp'.
See the variable `texmathp-tex-commands' about which commands are checked.
See the variable `texmathp-search-n-paragraphs' for limiting the search
\(there is no limit on the empty line (paragraph) search)."
  (interactive "p")
  (if (and (fboundp 'texmathp) (not (texmathp)))
      ;; jeste�my poza trybem matematycznym albo `texmathp' nie istnieje
      ad-do-it
    ;; IDEA: mo�na u�y� `insert' aby deaktywowa� `auto-fill-mode' itp.
    ;; w trybie matematycznym.
    (self-insert-command (or prefix 1))))

;; je�li `texmathp' jest ju� za�adowane, mo�emy go u�ywa�, w przeciwnym
;; wypadku wy��czamy (disable) porad� `tex-magic-space-texmathp' i dodajemy
;; jej automatyczne w��czanie po za�adowaniu pliku `texmathp'
(unless (or (featurep 'sierotki) 	; file was loaded already
	    (fboundp 'texmathp) (featurep 'texmathp))
  (ad-disable-advice 'tex-magic-space 'around 'tex-magic-space-texmathp)
  (eval-after-load "texmathp"
    '(ad-enable-advice 'tex-magic-space 'around 'tex-magic-space-texmathp)))


;; UWAGA: R�ne pliki r�nie definiuj� font (face) dla trybu matematycznego:
;; * AUCTeX: font-latex.el:  font-latex-math-face (LaTeX math expressions)
;; * AUCTeX: hilit-LaTeX.el: w�asne funkcje, u�ywa hilit19
;; * AUCTeX: tex-font.el:    tex-math-face (TeX math expressions)
;; * Emacs:  tex-mode.el:    tex-math-face (TeX math expressions)
;; ale mo�na u�ywa� `tex-math-face'.
;;
;; UWAGA: kolorowanie sk�adni w otoczeniach potrafi si� zmienia� po zmianie
;; wewn�trz (zapewne zwi�zane jest to z wieloliniowoscia tych wyra�e�)
;; cz�sto jedn� ze sk�adowych jest font-latex-math-face mimo wn�trza \mbox{}
;; czy \text{}; \ensuremath nie daje font-latex-math-face.
;;
;; UWAGA: otoczenie verbatim jest kolorowane font-latex-math-face.
(defvar tex-magic-space-face-list
  '(tex-math-face font-latex-math-face font-latex-sedate-face)
  "*List of faces when `tex-magic-space' should be inactive.
Defined in `tex-font' from AUCTeX and `tex-mode' from Emacs
* tex-math-face:          Face used to highlight TeX math expressions.
Defined in `font-latex' from AUCTeX:
* font-latex-math-face:   Face to use for LaTeX math environments.
* font-latex-sedate-face: Face to use for LaTeX minor keywords.

Used in advice `tex-magic-space-face'")

(defun nonempty-intersection (list-or-atom list)
  "Return non-nil if any element of LIST-OR-ATOM is element of LIST.
Comparison done with EQ using `memq'.  This version uses `while' to
iterate over elements of LIST-OR-ATOM."
  (let ((found)
	(elems list-or-atom))
    (if (atom list-or-atom)
	(memq list-or-atom list)
      (while (and elems (not found))
	(setq elems (cdr elems))
	(setq found (memq (car elems) list)))
      found)))
  
(defadvice tex-magic-space
  (around tex-magic-space-face (&optional prefix) preactivate)
  "Inactive when face belongs to `tex-magic-space-face-list'."
  (interactive "P")
  (if (not (nonempty-intersection (get-text-property (point) 'face)
				  tex-magic-space-face-list))
      ad-do-it
    (self-insert-command (or prefix 1))))

;; je�li u�ywamy kolorowania skladni (font lock) to mo�emy u�ywa� tej
;; porady, w przeciwnym wypadku wy��czamy (disable) porad�
;; `tex-magic-space-face' i dodajemy jej automatyczne w��czanie do hak�w
;; font-lock
(unless (or (featurep 'sierotki) 	; file was loaded already
	    (and (boundp 'global-font-lock-mode) global-font-lock-mode)
	    (and (boundp 'font-lock-mode) font-lock-mode))
  (ad-disable-advice 'tex-magic-space 'around 'tex-magic-space-face)
  (add-hook 'font-lock-mode-hook
	    #'(lambda () (ad-enable-advice 'tex-magic-space 'around
					   'tex-magic-space-face)))
  (add-hook 'global-font-lock-mode-hook
	    #'(lambda () (ad-enable-advice 'tex-magic-space 'around
					   'tex-magic-space-face))))

(defvar tex-magic-space-user-form
  'nil
  "*User defined form when `tex-magic-space' should be inactive.
You must set it before loading sierotki.el or [re]activate advices for
`tex-magic-space' after changing this variable using command
`tex-magic-space-toggle-checking' (`C-u \\[tex-magic-space-toggle-checking]').

Used in advice `tex-magic-space-user-form'")

(defadvice tex-magic-space
  (around tex-magic-space-user-form (&optional prefix) preactivate)
  "Inactive when `tex-magic-space-user-form' is non-nil."
  (interactive "P")
  (if (not (eval tex-magic-space-user-form))
      ad-do-it
    (self-insert-command (or prefix 1))))

;; je�li u�ytkownik nie zdefiniowa� `tex-magic-space-user-form' to czynimy
;; nieaktywn� (disable) porad� `tex-magic-space-user-form'
(unless tex-magic-space-user-form
  (ad-disable-advice 'tex-magic-space 'around 'tex-magic-space-user-form))


;;; ......................................................................
;;; Aktywacja porad i podobne
(defvar tex-magic-space-checking-string
  (when (ad-is-active 'tex-magic-space)
    (concat
     ":"
     (when (ad-advice-enabled
	    (ad-find-advice 'tex-magic-space 'around 'tex-magic-space-texmathp))
       "m")
     (when (ad-advice-enabled
	    (ad-find-advice 'tex-magic-space 'around 'tex-magic-space-face))
       "f")
     (when (ad-advice-enabled
	    (ad-find-advice 'tex-magic-space 'around 'tex-magic-space-user-form))
       "u")
     ))
  "Non-nil if advices for `tex-magic-space' are active.
Its value is string describing which advices are enabled.

Set by `tex-magic-space-toggle-checking'")

(defun tex-magic-space-toggle-checking (&optional arg)
  "Toggle whether `tex-magic-space' detects math mode.
With prefix argument ARG, activate detection if ARG is positive,
otherwise deactivate it.  Uses advice `tex-magic-space-texmathp'.
When advices are active, enabled advices are shown in the modeline after
TeX Magic Space mode string, `~'.

Sets `tex-magic-space-checking-string'.

See also: `tex-magic-space-texmathp', `tex-magic-space-face',
`tex-magic-space-user-form'"
  (interactive "P")
  ;; udost�pnij (enable) porad� `tex-magic-space-user-form' je�li
  ;; zmienna `tex-magic-space-user-form' jest r�ne od nil
  (if (null tex-magic-space-user-form)
      (ad-disable-advice 'tex-magic-space 'around 'tex-magic-space-user-form)
    (ad-enable-advice 'tex-magic-space 'around 'tex-magic-space-user-form))
  ;; zale�nie od warto�ci prefiksu i aktywno�ci porad aktywujemy lub
  ;; deaktywujemy _wszystkie_ porady do `tex-magic-space'.
  (cond ((null arg) (if (ad-is-active 'tex-magic-space)
			(ad-deactivate 'tex-magic-space)
		      (ad-activate 'tex-magic-space)))
	((> (prefix-numeric-value arg) 0) (ad-activate 'tex-magic-space))
	(t (ad-deactivate 'tex-magic-space)))
  ;; ustawiamy zmienn� `tex-magic-space-checking', kt�ra opisuje kt�re
  ;; porady s� w��czone; je�li porady s� niektywne jest r�wna nil.
  (setq tex-magic-space-checking-string
	(when (ad-is-active 'tex-magic-space)
	  (concat
	   ":"
	   (when (ad-advice-enabled
		(ad-find-advice 'tex-magic-space 'around 'tex-magic-space-texmathp))
	     "m")
	   (when (ad-advice-enabled
		(ad-find-advice 'tex-magic-space 'around 'tex-magic-space-face))
	     "f")
	   (when (ad-advice-enabled
		(ad-find-advice 'tex-magic-space 'around 'tex-magic-space-user-form))
	     "u")
	   )))
  (if tex-magic-space-mode
      (force-mode-line-update)
    (message "Advices for tex-magic-space %sctivated."
	     (if (ad-is-active 'tex-magic-space) "a" "dea"))))
	   
;; see also: `ad-is-advised', `ad-is-active', `ad-has-enabled-advice',
;;  `ad-get-enabled-advices', `ad-find-some-advice' and `ad-advice-enabled';       

;; TO DO:
;; 0. tex-magic-space-active, zawieraj�ca nil lub t, zale�nie od tego czy
;;    porady s� aktywne czy nie (aby zmienna tex-mabic-space-enabled-string
;;    nie pe�ni�a jednocze�nie dwu funkcji).
;; 1. tex-magic-space-enabled-string, zamieniaj�cy dost�pne porady na string.
;; 2. tex-magic-space-enabled-hist, zawieraj�ca histori� dla j.n.
;; 3. tex-magic-space-toggle-enabled, w��czaj�ca/wy��czaj�ca dost�pne porady,
;;    by� mo�e z wyj�tkiem tex-magic-space-user-form.
;; 4. tex-magic-space-check-user-form, sprawdzaj�ce czy zmienna
;;    tex-magic-space-user-form jest r�na od nil i odpowiednio
;;    aktywuj�ce/deaktywuj�ce porad� tex-magic-space-user-form.

;; TO DO: przejecha� si� po wszystkich advices (mo�na uwzgl�dnia� tylko
;; klas� around) np. za pomoc� ad-dolist, pobieraj�c je np. za pomoc�
;; (ad-get-advice-info-field 'tex-magic-space 'around), a nast�pnie
;; sprawdzi� jak si� porada nazywa za pomoc� `ad-advice-name' i czy jest
;; w��czona za pomoc� `ad-advice-enabled'.



;;; ----------------------------------------------------------------------
;;; Toggle magic space by Jakub Nar�bski <jnareb@fuw.edu.pl>,
;;; modifications based on code by Adam P. <adamp_at@at_ipipan.waw.pl>

;; Przypisuje/wy��cza przypisanie tex-magic-space do spacji,
;; (przydatne przy pisaniu matematyki)
;;;###autoload
(defun tex-toggle-magic-space (&optional arg)
  "Toggle whether SPC is bound to `tex-magic-space'.
With prefix argument ARG, bind SPC to `tex-magic-space' if ARG is positive,
otherwise bind SPC to `self-insert-command'.

It can be used to toggle temporarily `tex-magic-space' off when writing
equations (with e.g. `i' as index), then turn it on in main text.

Uses local keymap i.e. major mode keymap, so it currently works with
any mode, not only with LaTeX modes (there are several of them and
they do not use one common keymap).

See also command `tex-magic-space-mode' for an alternative way to use
`tex-magic-space'."
  (interactive "P")	                ; Prefix arg w postaci surowej.  Nie robi I/O.
  (let (space-binding
	local-space-binding)
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
    (setq space-binding       (key-binding " "))
    (setq local-space-binding (local-key-binding " "))
    (message "%s runs the command %s%s"
	     (key-description " ")
	     space-binding
	     ;; obs�uga przypadku gdy ta funkcja "nie dzia�a"
	     (if (or (and (null local-space-binding)
			  (eq space-binding 'self-insert-command))
		     (eq (key-binding " ") (local-key-binding " ")))
		 ""
	       (format "; locally bound to %s" (local-key-binding " ")))
	     ;; IDEA: mo�na u�y� minor-mode-key-binding do sprawdzenia
	     ;; w jakim trybie zosta�a zdefiniowana spacja.
	     )))


;;;; ======================================================================
;;;; `tex-magic-space-mode', mapa klawiatury, zarejestrowanie minor mode
(defvar tex-magic-space-mode nil
  "*Determines if TeX Magic Space mode is active.
You can set it directly or use the command `tex-magic-space-mode'.")
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
In this minor mode `\\[tex-magic-space]' runs the command `tex-magic-space'."
  (interactive "P")
;;;  (setq tex-magic-space-mode
;;;	(if (null arg) (not tex-magic-space-mode)
;;;	  (> (prefix-numeric-value arg) 0))))
  ;; w��cz lub wy��cz tryb
  (setq tex-magic-space-mode
	(not (or (and (null arg) tex-magic-space-mode)
		 (<= (prefix-numeric-value arg) 0))))
  ;; uaktualnij modeline
  ;; IDEA: mo�na by doda� informowanie o w(y)��czeniu tego trybu
  (force-mode-line-update))


;;; NOTES:
;;; * "Hide ifdef" mode z hideif.el u�ywa "pseudotrybu" `hide-ifdef-hiding' do
;;;   wy�wietlania opcjonalnego " Hiding", tzn. dodaje do `minor-mode-alist'
;;;   (hide-ifdef-hiding . " Hiding") opr�cz (hide-ifdef-mode . " Ifdef").
;;; * "CC Mode" analogicznie, dodaje (c-auto-hungry-string
;;;   . c-auto-hungry-string), gdzie c-auto-hungry-string to odpowiednio "/ah"
;;;   lub analogiczne; automagicznie si� zmienia.
;;; * elementami `minor-mode-alist' powinny by� pary (VARIABLE STRING), gdzie
;;;   STRING to mo�e by� (patrz `mode-line-format'):
;;;   - STRING, u�yty jak jest, z wykorzystaniem %-sth
;;;   - SYMBOL, u�yta jest jego warto�� (je�li r�na od t lub nil); %-sth
;;;     nie s� rozpoznawane gdy warto�ci� jest string
;;;   - (:eval FORM), FORM jest obliczana i umieszczany wynik (Emacs 21)
;;;   - (STRING REST...), (LIST REST...), oblicz rekurencyjnie i po��cz wyniki
;;;   - (SYMBOL THEN ELSE) lub (SYMBOL THEN), np. u�ycie `minor-mode-alist'
;;;   - (WIDTH REST...), dope�nione WIDTH spacjami je�li WIDTH > 0, skr�cony
;;;     do -WIDTH kolumn je�li WIDTH < 0; przyk�ad: (-3 "%p"), procent pliku
;;; * wi�kszo�� tryb�w "r�cznie" dodaje si� do modeline...

;;; 'Zarejestrowanie' trybu; na podstawie kodu z reftex.el
(if (fboundp 'add-minor-mode)
    ;; Je�li dost�pna jest funkcja `add-minor-mode' (w FSF Emacs jest to funkcja
    ;; kompatybilno�ci z XEmacsem, zdefiniowana w `subr'), to u�yj jej aby
    ;; uzyska� ekstra funkcjonalno��, tzn. wpis do minor mode menu w modeline.
    (progn
      ;; W�asno�� (property) :included ustala, czy dany trub jest widoczny w
      ;; minor mode menu w modeline.  Teoretycznie podana warto�� powinna
      ;; spowodowa� wpisanie do menu tylko dla podanych tryb�w; w FSF Emacs 21.2-7
      ;; jednak�e w�asno�� ta jest sprawdzana tylko przy wykonywaniu
      ;; `add-minor-mode'; w XEmacs 21.4.6-7 nie jest w og�le sprawdzana
      (put 'tex-magic-space-mode :included '(memq major-mode '(latex-mode
							       tex-mode)))
      ;; W�asno�� (property) :menu-tag podaje tekst pojawiaj�cy si� w minor mode
      ;; menu w modeline; w XEmacs 21.4.6-7 nie daje �adnego efektu, w minor
      ;; mode menu s� wszystkie minor mode, ten tryb jako "tex-magic-space-mode"
      ;; IDEA: mo�na by doda� do 'tex-magic-space-mode w�asno��
      ;; `menu-enable'; i tak (nie wiem dlaczego) nie dzia�a; mo�e FORM nie eval?
;;;   (put 'tex-magic-space-mode 'menu-enable '(memq major-mode '(latex-mode
;;;						                  tex-mode)))
      ;; je�li `add-minor-mode' u�ywa `menu-item' to u�y� w�asno�ci :visible
      ;; FORM lub :included FORM, :key-sequence KEY (aby przyspieszy� �adowanie)
      ;; NOTE: `add-minor-mode' u�ywa (define-key mode-line-menu... :button ...)
      (put 'tex-magic-space-mode :menu-tag "TeX Magic Space")
      ;; IDEA: tutaj mo�na by doda� za pomoc� funkcji `propertize' dodatkowe
      ;; w�asno�ci typu :help-echo, :local-map, :display czy :face
      (add-minor-mode 'tex-magic-space-mode
		      (list " ~" '(tex-magic-space-checking-string
				   tex-magic-space-checking-string))
		      tex-magic-space-mode-map))
  ;; Standardowy spos�b dodania minor mode, za "Emacs Lisp Reference Manual"
;;;(define-key mode-line-mode-menu
;;; (vector 'tex-magic-space-mode)
;;; ;; mo�na by u�y� ` do "cytowania" (quote) tylko cz�ci
;;; (list 'menu-item "TeX Magic Space"
;;;		'tex-magic-space-mode
;;;		:visible '(memq major-mode '(latex-mode tex-mode))
;;;		:button   (cons :toggle tex-magic-space-mode)))
  (unless (assq 'tex-magic-space-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(tex-magic-space-mode (" ~" (tex-magic-space-checking-string
					      tex-magic-space-checking-string)))
		;; (propertize " ~"
		;;	       'local-map mode-line-minor-mode-keymap
		;;	       'help-echo "mouse-3: minor mode menu")
		minor-mode-alist)))
  (unless (assq 'tex-magic-space-mode-map minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'tex-magic-space-mode tex-magic-space-mode-map)
		minor-mode-map-alist))))


;;;; ======================================================================
;;;; Inicjalizacja dla zapobiegania powstawaniu sierotek 'w locie'

;;; Initialization by Jakub Nar�bski <jnareb@fuw.edu.pl>
;;; and Adam Przepi�rkowski <adamp_at@at_ipipan.waw.pl>

;; Przypisz globalnie `tex-magic-space-mode' do `C-c SPC'
;; `mode-specific-map' to (globalna) mapa klawiatury dla prefiksu C-c
;; IDEA: `tex-magic-space-toggle-checking' mo�e by� w mapie dla TeX Magic
;; Space mode, tzn. w `tex-magic-space-mode-map'; jako prefiksu mo�na by
;; u�y� `C-c C-SPC', a jako klawiszy " ", "m", "f", "u".
(define-key mode-specific-map " " 'tex-magic-space-mode)
(define-key mode-specific-map "@" 'tex-magic-space-toggle-checking)
;; aby wpisa� 'C-SPC' trzeba u�y� wektora zamiast �a�cucha, t.j. [?\C- ]

;; TO DO: przepisa� to z powrotem na LaTeX-mode-hook, TeX-mode-hook,
;; reftex-mode-hook i tym podobne.  `define-key' dla odpowiedniej mapy
;; wystarczy zdefiniowa� raz w chwili gdy mapa jest dost�pna (za pomoc�
;; `eval-after-load') i domy�lnie w danym trybie we wszystkich buforach
;; `tex-magic-space' b�dzie w��czone lub nie.  `tex-magic-space-mode' (lub
;; ustawienie zmiennej) jest lokalne dla bufora (i takie powinno pozosta�),
;; wi�c nale�y doda� je do odpowiednich hak�w za pomoc� `add-hook' (uwaga:
;; jako argument pobiera on FUNCTION, a nie FORM!).

;; HAKI: reftex-mode-hook, reftex-load-hook (RefTeX), TeX-mode-hook,
;; LaTeX-mode-hook (AUCTeX, nieudokumentowane),
;; TeX-auto-prepare-hook/TeX-auto-cleanup-hook (AUCTeX), bibtex-mode-hook
;; (BibTeX), tex-mode-hook, plain-tex-mode-hook/latex-mode-hook (tex-mode);
;; uruchamia si� tak�e text-mode-hook (AUCTeX, tex-mode)

;; W��cz TeX Magic Space mode dla znanych tryb�w (La)TeX-owych
(defmacro tex-magic-space-mode-add-to-hook (hook)
  "Add `(setq 'tex-magic-space-mode t)' to HOOK."
  `(add-hook ,hook (function (lambda () (setq tex-magic-space-mode t)))))

(defmacro tex-magic-space-mode-initialize (hooks)
  "Add `(setq 'tex-magic-space-mode t)' to each of HOOKS."
  `(dolist (hook ,hooks)
     (tex-magic-space-mode-add-to-hook hook)))

(defvar tex-magic-space-mode-hooks-list
  '(TeX-mode-hook LaTeX-mode-hook 	; for AUCTeX
    tex-mode-hook			; for tex-mode
    reftex-mode-hook			; for RefTeX minor mode
    bibtex-mode-hook)			; for BibTeX
  "List of hooks to which add turning on TeX Magic Space minor mode.
You must set this using (setq tex-magic-space-mode-hooks-list VALUE) before
loading this file i.e. before (require 'sierotki).")

(tex-magic-space-mode-initialize tex-magic-space-mode-hooks-list)


;;;; ======================================================================
;;;; Zako�czenie
;; Aby mo�na by�o �adowa� ten plik zar�wno za pomoc�
;; (load "sierotki") jak i (requires 'sierotki)

;;; *** Announce ***

(provide 'sierotki)

;; Local variables:
;; coding: iso-latin-2
;; End:

;;; sierotki.el ends here
