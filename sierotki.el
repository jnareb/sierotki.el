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
(defun tex-hard-spaces ()
  "Replaces space after single-letter word with '~', 
the TeX non-breakable space"
 (interactive)
 (query-replace-regexp "\\<\\([wiazouWIAZOU]\\)\\s +"
                       "\\1~"))

;;; ---------------------------------------------------------------------- 
;;; Magic space by Michal Jankowski <michalj@fuw.edu.pl>
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

;; 'Rêcznie' mo¿na j± w³±czyæ za pomoc±
;; M-x local-set-key SPC tex-magic-space
;; albo (obecnie) za pomoc± C-c SPC.
;; UWAGA: Dzia³a gdy u¿ywamy AUCTeX-a; dla latex-mode nale¿y zmieniæ
;;        TeX-mode-map na tex-mode-map
(defun tex-toggle-magic-space ()
  "Toggles TeX magic space, which inserts non-breakable space after a
single-letter word"
  (interactive)
  (progn
    (if (equal (lookup-key TeX-mode-map " ") 'tex-magic-space)
        (progn
          (define-key TeX-mode-map " " nil)
          (local-unset-key " ")) ; to be sure
      (define-key TeX-mode-map " " 'tex-magic-space))
    (message "SPC is binded to %s" (lookup-key TeX-mode-map " "))))
;; C-c SPC toggles magic space:
;; mode-specific-map is keymap for characters following C-c
;;; From: adamp_at@at_ipipan.waw.pl (Adam P.)
(define-key mode-specific-map " " 'tex-toggle-magic-space) ; C-c SPC

;; For AUC TeX
(eval-after-load "tex" '(define-key TeX-mode-map " " 'tex-magic-space))
;; For tex-mode as included in Emacs
(eval-after-load "tex-mode" '(define-key tex-mode-map " " 'tex-magic-space))
;; For RefTeX
(add-hook 'reftex-mode-hook
          (function
           (lambda ()
              (define-key reftex-mode-map " " 'tex-magic-space))))

;;; sierotki.el ends here
