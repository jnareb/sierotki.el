;;; sierotki.el --- Introduce tildes after single-letter words

;; Copyright (C) 2002  Michal Jankowski, Jakub Narebski

;; Author: Michal Jankowski <michalj@fuw.edu.pl>
;;	Jakub Narebski <jnareb@fuw.edu.pl>
;;	Adam P. <adamp_at@at_ipipan.waw.pl>
;; Maintainer: Jakub Narebski <jnareb@fuw.edu.pl>
;; Version: 1.0
;; Keywords: tex, wp
;; Created: 

;; $Id$

;;; Commentary:

;;; Code:

;;; ---------------------------------------------------------------------- 
;;; From: rysiek@IPIPAN.GDA.PL (Ryszard Kubiak)
;; Introduce (interactively) tilde characters (\TeX's hard-space ~)
;; after one-letter Polish articles in the entire buffer
(defun hard-spaces ()
 (interactive)
 (query-replace-regexp "\\([ \C-j\C-m]+\\)\\([wiazouWIAZOU]\\)[ \C-j\C-m]+"
                       "\\1\\2~"))

;;; ---------------------------------------------------------------------- 
;;; From: Michal Jankowski <michalj@fuw.edu.pl>
(defun tex-magic-space () 
"Magic-space - inserts non-breakable space after a single-letter word." 
  (interactive)
  (if (string-match 
       "^\\(\\s \\|~\\)?[aeiouwz]$" 
       (buffer-substring (max (point-min) (- (point) 2)) (point)))
      (insert "~")
    (insert " ")
    (and auto-fill-function
	 (funcall auto-fill-function))))

;; For AUC TeX
(eval-after-load "tex" '(define-key TeX-mode-map " " 'tex-magic-space))
;; For tex-mode as included in Emacs
(eval-after-load "tex-mode" '(define-key tex-mode-map " " 'tex-magic-space))

;;; sierotki.el ends here
