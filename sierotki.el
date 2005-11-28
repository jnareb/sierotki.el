;;; sierotki.el --- Introduce tildes after single-letter words
;;
;; Copyright (C) 1999-2005  Micha³ Jankowski, Jakub Narêbski
;; 
;; Authors: Ryszard Kubiak <rysiek@ipipan.gda.pl>
;;	Micha³ Jankowski <michalj@fuw.edu.pl>
;;	Jakub Narêbski <jnareb@fuw.edu.pl>
;; Maintainer: Jakub Narêbski <jnareb@fuw.edu.pl>
;; Created: 3 Nov 1999
;;
;; Last-Updated: Tue Nov 29 00:31:08 2005 (3600 CET)
;;           By: Jakub Narebski
;;     Update #: 11
;;
;; Version: 2.6.7
;; RCS Id: $Id$
;; RCS Version:	$Revision$
;; RCS Date: $Date$
;; Keywords: TeX, wp, convenience
;; URL: http://www.fuw.edu.pl/~jnareb/sierotki.el
;;      http://www.emacswiki.org/emacs/sierotki.el
;; EmacsWiki: NonbreakableSpace
;;
;; Compatibility: Emacs21, XEmacs21
;; Incompatibility:

;; $Id$

;; This file is *NOT* part of GNU Emacs.
;; This file is distributed under the same terms as GNU Emacs.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
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

;;; Installation[pl]:

;; Aby u¿yæ tego pakietu, umie¶æ nastêpuj±c± linijkê w swoim pliku .emacs
;;
;;    (require 'sierotki)
;;
;; Je¶li nie chcesz go ³adowaæ zanim nie bêdzie potrzebny, mo¿esz u¿yæ
;; ³adowania na ¿±danie, np. dodaj±c do .emacs nastêpuj±ce linie
;;
;;    (autoload 'tex-magic-space-mode "sierotki"
;;              "TeX Magic Space minor mode" t)
;;    (define-key mode-specific-map " " 'tex-magic-space-mode)
;;
;; Wówczas po wci¶niêciu `C-c SPC' zostanie w³±czony TeX Magic Space mode
;; i zostanie za³adowana reszta funkcji.  Uwaga: przy u¿ywaniu
;; automatycznego ³adowania ten tryb _nie mo¿e_ byæ automatycznie w³±czany
;; w trybach LaTeX-owych.
;;
;; Je¶li chcesz by TeX Magic Space mode by³ automatycznie w³±czany
;; w znanych trybach TeX-owych dodaj nastêpuj±c± linijkê do swojego pliku
;; .emacs po (require 'sierotki)
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
;; language, those here are for Polish).
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

;; Documentation and comments: Jakub Narêbski.

;;; Description[pl]:

;; Ten pakiet s³u¿y do dowi±zywania zdefiniowanych wyrazów (domy¶lnie
;; jednoliterowych spójników) do nastêpuj±cych po nich s³ów za pomoc± znaku
;; `~' (tyldy), nie³amliwej spacji TeX-owej.  S³u¿y to temu, aby w
;; dokumentach TeX-owych unikn±æ jednoliterowych spójników na koñcach linii,
;; co jest wymagane przez polskie (i czeskie) regu³y typograficzne.
;;
;; Pakiet ten dostarcza dwu funkcjonalno¶ci.  Pierwsz± z nich jest
;; sprawdzenie (istniej±cego) tekstu i zasugerowanie dodania brakuj±cych
;; tyld.  Jest ona implementowana przez komendê `tex-hard-spaces', za pomoc±
;; `query-replace-regexp'.  Tê sam± (a nawet rozszerzon±) funkcjonalno¶æ
;; znale¼æ mo¿na w pakiecie `tildify' (UWAGA: domy¶lne ustawienia w tym
;; pakiecie s± dostosowane do jêzyka czeskiego).
;;
;; Drug± z funkcjonalno¶ci jest automatyczne wpisywanie tyld po
;; jednoliterowych spójnikach podczas pisania tekstu (w locie).  Jest ona
;; implementowana przez komendê `tex-magic-space', któr± nale¿y podpi±æ do
;; spacji.  Do aktywowania tej funkcjonalno¶ci nale¿y w³±czyæ
;; `tex-magic-space-mode'.  Tryb (minor mode) TeX Magic Space mo¿na aktualnie
;; w³±czyæ tak¿e z modeline minor mode menu; jest on oznaczany za pomoc± " ~".
;; Ewentualne dodatkowe oznaczenia po " ~" informuj±, ¿e porady/testy s±
;; aktywne.
;;
;; Funkcjonalno¶æ ta mo¿e byæ automatycznie w³±czana w trybach TeX-owych za
;; pomoc± dodania odpowiednika `turn-on-tex-magic-space-mode' do odpowiednich
;; haczyków (zdefiniowanych w zmiennej `tex-magic-space-mode-hooks-list') za
;; pomoc± polecenia (funkcji) `turn-on-tex-magic-space-in-tex-modes'.

;; Dokumentacja i komentarze: Jakub Narêbski.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 4-Nov-2005    Jakub Narebski  
;;    Last-Updated: Thu Nov  3 23:47:10 2005 #6 (Jakub Narebski)
;;    

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

;;; Change Log[pl]:

;; Wersja 1.2 (RCS revision 1.2):
;; * Dodano `tex-toggle-magic-space'.
;; Wersja 1.3 (RCS revision 1.4):
;; * Wyra¿enia regularne w zmiennych, a nie zapisane wewn±trz funkcji.
;; Wersja 2.0 (RCS revision 1.6):
;; * Nowa implementacja `tex-magic-space'.
;; Wersja 2.3 (RCS revision 1.12):
;; * Pojawi³ siê TeX Magic Space minor mode (przypisany do `C-c SPC').
;; Wersja 2.4 (RCS revision 1.17):
;; * Dodane porady i polecenie do ich w³±czana (przypisane do `C-c @'), aby
;;   `tex-magic-space' pozostawa³a nieaktywna tam gdzie nie trzeba (np.
;;   w trybie matematycznym wykrywanym za pomoc± `texmathp').
;; Wersja 2.5 (RCS revision 1.26):
;; * Usuniêcie `tex-toggle-magic-space'; u¿yj `tex-magic-space-mode'. 
;; Wersja 2.6 (RCS revision 1.31):
;; * Sprawdzania czy `tex-magic-space' powinno byæ nieaktywne zosta³o
;;   przepisane za pomoc± instrukcji warunkowej w g³ównej funkcji zamiast
;;   u¿ywania do tego porad (advice).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


;;;; ======================================================================
;;;; Add non-breakable spaces in existing document, interactively.
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

;; Zwyk³e `query-replace-regexp', czyli C-M-% dla odpowiedniego
;; wyra¿enia regularnego, zapisanego w `tex-hard-spaces-regexp'
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
;;;; Zapobieganie powstawaniu sierotek 'w locie'

;;; Magic space by Michal Jankowski <michalj@fuw.edu.pl>
;;; Modified by Jakub Narêbski <jnareb@fuw.edu.pl>


;;; ----------------------------------------------------------------------
;;; Tests for `tex-magic-space'
;;; Testy dla `tex-magic-space'

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
	 ;;(or (insert "!") t)
	 (setq end (and (skip-chars-forward (concat "^" delim)
					    (point-at-eol))
			(point)))
	 (or (eolp)
	     (looking-at (concat "[" delim "]")))
	 ;;(or (insert "!") t)
	 (cond ((>= point end) nil)
	       ((eolp) beg)
	       (t (cons beg end)))))))

;;; ......................................................................
;;; Turning on tests for tex-magic-space
;;; Aktywacja sprawdzania/testów dla tex-magic-space i podobne
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
;;; Wstawianie tyld w locie

;; UWAGA: [czasami] polskie literki s± traktowane jako koniec s³owa dla 8bit
;;        tzn. przy u¿yciu `standard-display-european' do ich wprowadzania.
;;        Bêdê próbowac znale¼æ dok³adne warunki wyst±pienia b³edu.
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
  (interactive "p")	               ; Prefix arg jako liczba.  Nie robi I/O.
  ;; Tests
  (unless (and tex-magic-space-do-checking
	       (some (lambda (f) (and (fboundp f) (funcall f)))
		     tex-magic-space-tests))
    ;; tests failed
    (when (string-match
	   tex-magic-space-regexp      ; wyra¿enie rozpoznaj±ce samotne spójniki
	   (buffer-substring (max (point-min) (- (point) 2)) (point)))
      (setq last-command-char ?~)))    ; wstawiamy `~' zamiast SPC
  (self-insert-command (or prefix 1))) ; daje obs³ugê auto-fill, abbrev, blinkin-paren

(defun debug-tex-magic-space (&optional prefix)
  "Version of `tex-magic-space' which does'n do any testing."
  (interactive "p")
  (let ((tex-magic-space-do-checking nil))
    (tex-magic-space prefix)))


;;; ----------------------------------------------------------------------
;;; The TeX Magic Space mode definition and initialization
;;; Definicja trybu i inicjalizacja


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
  ;; alternative, less clear and slower
;;;  (setq tex-magic-space-mode
;;;	(not (or (and (null arg) tex-magic-space-mode)
;;;		 (<= (prefix-numeric-value arg) 0))))
  ;; uaktualnij modeline
  ;; IDEA: mo¿na by dodaæ informowanie o w(y)³±czeniu tego trybu
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


;;; NOTES:
;;; * "Hide ifdef" mode z hideif.el u¿ywa "pseudotrybu" `hide-ifdef-hiding' do
;;;   wy¶wietlania opcjonalnego " Hiding", tzn. dodaje do `minor-mode-alist'
;;;   (hide-ifdef-hiding . " Hiding") oprócz (hide-ifdef-mode . " Ifdef").
;;; * "CC Mode" analogicznie, dodaje (c-auto-hungry-string
;;;   . c-auto-hungry-string), gdzie c-auto-hungry-string to odpowiednio "/ah"
;;;   lub analogiczne; automagicznie siê zmienia.
;;; * elementami `minor-mode-alist' powinny byæ pary (VARIABLE STRING), gdzie
;;;   STRING to mo¿e byæ (patrz `mode-line-format'):
;;;   - STRING, u¿yty jak jest, z wykorzystaniem %-sth
;;;   - SYMBOL, u¿yta jest jego warto¶æ (je¶li ró¿na od t lub nil); %-sth
;;;     nie s± rozpoznawane gdy warto¶ci± jest string
;;;   - (:eval FORM), FORM jest obliczana i umieszczany wynik (Emacs 21)
;;;   - (STRING REST...), (LIST REST...), oblicz rekurencyjnie i po³±cz wyniki
;;;   - (SYMBOL THEN ELSE) lub (SYMBOL THEN), np. u¿ycie `minor-mode-alist'
;;;   - (WIDTH REST...), dope³nione WIDTH spacjami je¶li WIDTH > 0, skrócony
;;;     do -WIDTH kolumn je¶li WIDTH < 0; przyk³ad: (-3 "%p"), procent pliku
;;; * wiêkszo¶æ trybów "rêcznie" dodaje siê do modeline...

;;; 'Zarejestrowanie' trybu; na podstawie kodu z reftex.el
(if (fboundp 'add-minor-mode)
    ;; Je¶li dostêpna jest funkcja `add-minor-mode' (w FSF Emacs jest to funkcja
    ;; kompatybilno¶ci z XEmacsem, zdefiniowana w `subr'), to u¿yj jej aby
    ;; uzyskaæ ekstra funkcjonalno¶æ, tzn. wpis do minor mode menu w modeline.
    (progn
      ;; W³asno¶æ (property) :included ustala, czy dany trub jest widoczny w
      ;; minor mode menu w modeline.  Teoretycznie podana warto¶æ powinna
      ;; spowodowaæ wpisanie do menu tylko dla podanych trybów; w FSF Emacs 21.2-7
      ;; jednak¿e w³asno¶æ ta jest sprawdzana tylko przy wykonywaniu
      ;; `add-minor-mode'; w XEmacs 21.4.6-7 nie jest w ogóle sprawdzana
      (put 'tex-magic-space-mode :included '(memq major-mode '(latex-mode
							       tex-mode)))
      ;; W³asno¶æ (property) :menu-tag podaje tekst pojawiaj±cy siê w minor mode
      ;; menu w modeline; w XEmacs 21.4.6-7 nie daje ¿adnego efektu, w minor
      ;; mode menu s± wszystkie minor mode, ten tryb jako "tex-magic-space-mode"
      ;; IDEA: mo¿na by dodaæ do 'tex-magic-space-mode w³asno¶æ
      ;; `menu-enable'; i tak (nie wiem dlaczego) nie dzia³a; mo¿e FORM nie eval?
;;;   (put 'tex-magic-space-mode 'menu-enable '(memq major-mode '(latex-mode
;;;						                  tex-mode)))
      ;; je¶li `add-minor-mode' u¿ywa `menu-item' to u¿yæ w³asno¶ci :visible
      ;; FORM lub :included FORM, :key-sequence KEY (aby przyspieszyæ ³adowanie)
      ;; NOTE: `add-minor-mode' u¿ywa (define-key mode-line-menu... :button ...)
      (put 'tex-magic-space-mode :menu-tag "TeX Magic Space")
      ;; IDEA: tutaj mo¿na by dodaæ za pomoc± funkcji `propertize' dodatkowe
      ;; w³asno¶ci typu :help-echo, :local-map, :display czy :face
      (add-minor-mode 'tex-magic-space-mode
		      (list " ~" '(tex-magic-space-do-checking ":Chk"))
		      tex-magic-space-mode-map))
  ;; Standardowy sposób dodania minor mode, za "Emacs Lisp Reference Manual"
;;;(define-key mode-line-mode-menu
;;; (vector 'tex-magic-space-mode)
;;; ;; mo¿na by u¿yæ ` do "cytowania" (quote) tylko czê¶ci
;;; (list 'menu-item "TeX Magic Space"
;;;		'tex-magic-space-mode
;;;		:visible '(memq major-mode '(latex-mode tex-mode))
;;;		:button   (cons :toggle tex-magic-space-mode)))
  (unless (assq 'tex-magic-space-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(tex-magic-space-mode (" ~" (tex-magic-space-do-checking ":Chk")))
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

;;; Initialization by Jakub Narêbski <jnareb@fuw.edu.pl>
;;; and Adam Przepiórkowski <adamp_at@at_ipipan.waw.pl>

;; Przypisz globalnie `tex-magic-space-mode' do `C-c SPC'
;; `mode-specific-map' to (globalna) mapa klawiatury dla prefiksu C-c
;; IDEA: `tex-magic-space-toggle-checking' mo¿e byæ w mapie dla TeX Magic
;; Space mode, tzn. w `tex-magic-space-mode-map'; jako prefiksu mo¿na by
;; u¿yæ `C-c C-SPC', a jako klawiszy " ", "m", "f", "u".
(define-key mode-specific-map " " 'tex-magic-space-mode)
;(define-key mode-specific-map "@" 'tex-magic-space-toggle-checking)
;; aby wpisaæ 'C-SPC' trzeba u¿yæ wektora zamiast ³añcucha, t.j. [?\C- ]

;; TO DO: przepisaæ to z powrotem na LaTeX-mode-hook, TeX-mode-hook,
;; reftex-mode-hook i tym podobne.  `define-key' dla odpowiedniej mapy
;; wystarczy zdefiniowaæ raz w chwili gdy mapa jest dostêpna (za pomoc±
;; `eval-after-load') i domy¶lnie w danym trybie we wszystkich buforach
;; `tex-magic-space' bêdzie w³±czone lub nie.  `tex-magic-space-mode' (lub
;; ustawienie zmiennej) jest lokalne dla bufora (i takie powinno pozostaæ),
;; wiêc nale¿y dodaæ je do odpowiednich haków za pomoc± `add-hook' (uwaga:
;; jako argument pobiera on FUNCTION, a nie FORM!).

;; HAKI: reftex-mode-hook, reftex-load-hook (RefTeX), TeX-mode-hook,
;; LaTeX-mode-hook (AUCTeX, nieudokumentowane),
;; TeX-auto-prepare-hook/TeX-auto-cleanup-hook (AUCTeX), bibtex-mode-hook
;; (BibTeX), tex-mode-hook, plain-tex-mode-hook/latex-mode-hook (tex-mode);
;; uruchamia siê tak¿e text-mode-hook (AUCTeX, tex-mode)

;; W³±cz TeX Magic Space mode dla znanych trybów (La)TeX-owych
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
;;;; Zakoñczenie

(provide 'sierotki)

;; Local variables:
;; coding: iso-latin-2
;; End:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sierotki.el ends here
