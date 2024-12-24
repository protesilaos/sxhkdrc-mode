;;; sxhkdrc-mode.el --- Major mode for sxhkdrc files (Simple X Hot Key Daemon) -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/sxhkdrc-mode
;; Version: 1.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Major mode for editing sxhkdrc files (Simple X Hot Key Daemon).  It
;; defines basic fontification rules and supports indentation.
;;
;; SXHKD is the Simple X Hot Key Daemon which is commonly used in
;; minimalist desktop sessions on Xorg, such as with the Binary Space
;; Partitioning Window Manager (BSPWM).
;;
;; Why call the package SXHKDRC-something?  One school of thought is
;; that it is named after the files it applies to.  The heterodox
;; view, however, is that SXHKDRC is a backronym: Such Xenotropic Hot
;; Keys Demonstrate Robustness and Configurability.

;;; Code:

(defgroup sxhkdrc nil
  "Major mode for editing sxhkdrc files.
SXHKD is the Simple X Hotkey Daemon which is commonly used in
minimalist desktop sessions on Xorg, such as with the Binary
Space Partitioning Window Manager (BSPWM)."
  :group 'programming)

(defvar sxhkdrc-mode-syntax
  '((key-modifier . ( "control" "ctrl" "shift" "alt" "meta" "super" "hyper"
                      "mod1" "mod2" "mod3" "mod4" "mod5"))
    (key-generic . "^\\({.*?}\\|\\<.*?\\>\\)")
    (key-line . "^\\({.*?}\\|\\<.*?\\>\\).*$")
    (outline . "\\(####* [^\s\t\n]\\|{.*?}\\|\\<.*?\\>\\)")
    (comment . "^\\([\s\t]+\\)?#.*$")
    (command . "^[\s\t]+\\([;]\\)?\\(\\_<.*?\\_>\\)")
    (command-line . "^[\s\t]+\\([;]\\)?\\(\\_<.*?\\_>\\).*$")
    (indent-other . 0)
    (indent-command . 4))
  "List of associations for sxhkdrc syntax.")

(defvar sxhkdrc-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?. "_" table)
    table)
  "Syntax table for `sxhkdrc-mode'.")

(defun sxhkdrc-mode--modifiers-regexp (placement)
  "Return `sxhkdrc-mode--modifiers' as a single string regexp.
PLACEMENT controls how to format the regexp: `start' is for the
beginning of the line, `chord' is when the modifier is part of a
key chord chain (demarcated by a colon or semicolon)."
  (let ((mods (alist-get 'key-modifier sxhkdrc-mode-syntax)))
    (pcase placement
      ('start (format "^\\(%s\\)" (mapconcat #'identity mods "\\|")))
      ('chord (format "[;:]\\([\s\t]\\)?\\(%s\\)" (mapconcat #'identity mods "\\|"))))))

(defface sxhkdrc-mode-primary-modifier
  '((t :inherit font-lock-keyword-face))
  "Face for sxhkd modifiers at the start of a key sequence or chord.")

(defface sxhkdrc-mode-generic-key
  '((t :inherit font-lock-builtin-face))
  "Face for sxhkd generic keys at the start of a sequence.")

(defface sxhkdrc-mode-command
  '((t :inherit font-lock-function-name-face))
  "Face for the first part of an sxhkd command.")

(defface sxhkdrc-mode-command-async
  '((t :inherit bold))
  "Face for the sxhkd asynchronous command indicator.")

(defconst sxhkdrc-mode-font-lock-keywords
  (let ((syntax sxhkdrc-mode-syntax))
    `((,(sxhkdrc-mode--modifiers-regexp 'start)
       (1 'sxhkdrc-mode-primary-modifier))
      (,(sxhkdrc-mode--modifiers-regexp 'chord)
       (2 'sxhkdrc-mode-primary-modifier))
      (,(alist-get 'command syntax)
       (1 'sxhkdrc-mode-command-async t t)
       (2 'sxhkdrc-mode-command t t))
      (,(alist-get 'comment syntax)
       (0 'font-lock-comment-face t t))
      (,(alist-get 'key-generic syntax)
       (0 'sxhkdrc-mode-generic-key))))
  "Fontification of sxhkdrc files.")

(defun sxhkdrc-mode-indent-line ()
  "Indent line according to `sxhkdrc-mode-syntax'."
  (interactive)
  (let* ((syntax sxhkdrc-mode-syntax)
         (command (alist-get 'command syntax))
         (key (alist-get 'key-generic syntax))
         (indent-other (alist-get 'indent-other syntax))
         (indent-command (alist-get 'indent-command syntax))
         indent)
    ;; The `or' statements here are needed because this will work with
    ;; `electric-indent-mode' that does RET+TAB in one go.
    (save-excursion
      (goto-char (line-beginning-position))
      (skip-chars-forward "\t " (line-end-position))
      (cond
       ;; If the command continues to a new line by virtue of a
       ;; trailing \ then we indent accordingly.
       ((or (looking-at command)
            (progn
              (forward-line -1)
              (beginning-of-line)
              (or (re-search-forward ".*\\\\$" (line-end-position) t)
                  (looking-at key))))
        (setq indent indent-command))
       ;; If the previous line is a command that does not end with a
       ;; backslash, we want to reset indentation.
       ((or (looking-at command)
            (progn
              (forward-line -1)
              (beginning-of-line)
              (looking-at command)))
        (setq indent indent-other))
       ;; If we are on a key definition, the following will be a
       ;; command.
       ((or (looking-at key)
            (progn
              (forward-line -1)
              (beginning-of-line)
              (looking-at key)))
        (setq indent indent-command))))
    (if indent
        (progn
          (delete-horizontal-space)
          (indent-to indent))
      'no-indent)))

(defvar sxhkdrc-mode-map (make-sparse-keymap)
  "Local keymap for `sxhkdrc-mode' buffers.")

(defun sxhkdrc-outline-level ()
  "The value of variable `outline-level' for `sxhkdrc-mode'."
  ;; Expects outline-regexp is "\\(####* [^\s\t\n]\\|{.*?}\\|\\<.*?\\>\\)"
  ;; and point is at the beginning of a matching line.
  (let ((len (- (match-end 0) (match-beginning 0))))
    (cond
     ((looking-at-p "\\({.*?}\\|\\<.*?\\>\\)")
      1000)
     ((looking-at "##\\(#+\\) ")
      (- (match-end 1) (match-beginning 1)))
     ;; Above should match everything but just in case.
     (t
      len))))

;;;###autoload
(define-derived-mode sxhkdrc-mode fundamental-mode "SXHKDRC"
  "Major mode for editing sxhkdrc files (Simple X Hot Key Daemon)."
  :syntax-table sxhkdrc-mode-syntax-table
  (setq-local indent-line-function 'sxhkdrc-mode-indent-line
              comment-start "#"
              comment-start-skip (concat (regexp-quote comment-start) "+\\s *")
              outline-regexp (alist-get 'outline sxhkdrc-mode-syntax)
              outline-level 'sxhkdrc-outline-level
              imenu-generic-expression `(("Command" ,(alist-get 'command-line sxhkdrc-mode-syntax) 0)
                                         ("Key" ,(alist-get 'key-line sxhkdrc-mode-syntax) 0))
              font-lock-defaults '(sxhkdrc-mode-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("sxhkdrc\\'" . sxhkdrc-mode))

(provide 'sxhkdrc-mode)
;;; sxhkdrc-mode.el ends here
