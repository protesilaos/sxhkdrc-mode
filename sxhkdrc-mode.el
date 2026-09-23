;;; sxhkdrc-mode.el --- Major mode for sxhkdrc files (Simple X Hot Key Daemon) -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026  Free Software Foundation, Inc.

;; Author: Protesilaos <info@protesilaos.com>
;; Maintainer: Protesilaos <info@protesilaos.com>
;; URL: https://github.com/protesilaos/sxhkdrc-mode
;; Version: 1.2.0
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
;; Partitioning Window Manager (BSPWM) or HerbstluftWM.
;;
;; Why call the package SXHKDRC-something?  One school of thought is
;; that it is named after the files it applies to.  The heterodox
;; view, however, is that SXHKDRC is a backronym: Such Xenotropic Hot
;; Keys Demonstrate Robustness and Configurability.

;;; Code:

(eval-when-compile (require 'subr-x))

(defgroup sxhkdrc nil
  "Major mode for editing sxhkdrc files.
SXHKD is the Simple X Hotkey Daemon which is commonly used in
minimalist desktop sessions on Xorg, such as with the Binary
Space Partitioning Window Manager (BSPWM) or HerbstluftWM."
  :group 'programming)

(defvar sxhkdrc-mode-syntax
  '((key-modifier . ( "super" "hyper" "meta" "alt" "control" "ctrl" "shift"
                      "mode_switch" "lock" "mod1" "mod2" "mod3" "mod4" "mod5" "any"))
    (key-generic . "^\\({.*?}\\|\\<.*?\\>\\)")
    (key-line . "^\\({.*?}\\|\\<.*?\\>\\).*$")
    (key-chain-one-off . " \\(;\\) ")
    (key-chain-continuous . " \\(:\\) ")
    (outline . "\\(####* [^\s\t\n]\\|{.*?}\\|\\<.*?\\>\\)")
    (comment . "^\\([\s\t]+\\)?#.*$")
    (command . "^[\s\t]+\\([;@~]\\)?\\(\\_<.*?\\_>\\)")
    (command-line . "^[\s\t]+\\([;@~]\\)?\\(\\_<.*?\\_>\\).*$")
    (continuation-line . "\\\\$")
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
  (if-let* ((mods (alist-get 'key-modifier sxhkdrc-mode-syntax)))
      (pcase placement
        ('start (format "^\\(%s\\)" (mapconcat #'identity mods "\\|")))
        ('chord (format "[;:]\\([\s\t]\\)?\\(%s\\)" (mapconcat #'identity mods "\\|"))))
    (error "Cannot find modifiers in `sxhkdrc-mode-syntax'")))

(defface sxhkdrc-mode-primary-modifier
  '((t :inherit font-lock-keyword-face))
  "Face for modifiers at the start of a key sequence or chord.")

(defface sxhkdrc-mode-generic-key
  '((t :inherit font-lock-builtin-face))
  "Face for generic keys at the start of a sequence.")

(defface sxhkdrc-mode-command
  '((t :inherit font-lock-function-name-face))
  "Face for the first part of a command.")

(defface sxhkdrc-mode-command-prefix
  '((t :inherit font-lock-type-face))
  "Face for the command prefix indicator: [;@~].")

(defface sxhkdrc-mode-continuation-line
  '((t :inherit font-lock-constant-face))
  "Face for continuation lines in commands (backslash at the end of line).")

(defface sxhkdrc-mode-chain-one-off-separator
  '((t :inherit success))
  "Face for the key chord chain one-off separator.")

(defface sxhkdrc-mode-chain-continuous-separator
  '((t :inherit warning))
  "Face for the key chord chain continuous separator.")

(defconst sxhkdrc-mode-font-lock-keywords
  (let ((syntax sxhkdrc-mode-syntax))
    `((,(sxhkdrc-mode--modifiers-regexp 'start)
       (1 'sxhkdrc-mode-primary-modifier))
      (,(sxhkdrc-mode--modifiers-regexp 'chord)
       (2 'sxhkdrc-mode-primary-modifier))
      (,(alist-get 'command syntax)
       (1 'sxhkdrc-mode-command-prefix t t)
       (2 'sxhkdrc-mode-command t t))
      (,(alist-get 'comment syntax)
       (0 'font-lock-comment-face t t))
      (,(alist-get 'key-generic syntax)
       (0 'sxhkdrc-mode-generic-key))
      (,(alist-get 'key-chain-one-off syntax)
       (1 'sxhkdrc-mode-chain-one-off-separator t t))
      (,(alist-get 'key-chain-continuous syntax)
       (1 'sxhkdrc-mode-chain-continuous-separator t t))
      (,(alist-get 'continuation-line syntax)
       (0 'sxhkdrc-mode-continuation-line))))
  "Fontification of sxhkdrc files.")

(defun sxhkdrc-mode-indent-line ()
  "Indent line according to `sxhkdrc-mode-syntax'."
  (interactive)
  (let* ((syntax sxhkdrc-mode-syntax)
         (command (alist-get 'command syntax))
         (key (alist-get 'key-generic syntax))
         (indent-other (alist-get 'indent-other syntax))
         (indent-command (alist-get 'indent-command syntax))
         (indent nil))
    ;; The `or' statements here are needed because this will work with
    ;; `electric-indent-mode' that does RET+TAB in one go.
    (save-excursion
      (goto-char (line-beginning-position))
      (skip-chars-forward "\t " (line-end-position))
      (cond
       ;; If the command continues to a new line by virtue of a
       ;; trailing \ then we indent accordingly.
       ((or (looking-at command)
            (and-let* ((continuation (alist-get 'continuation-line sxhkdrc-mode-syntax)))
              (progn
                (forward-line -1)
                (beginning-of-line)
                (or (re-search-forward continuation (line-end-position) t)
                    (looking-at key)))))
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

;; TODO 2026-09-23: Can `sxhkdrc-mode-restart-process' be done without
;; `shell-command-to-string'?  I tried (process-id "sxhkd") but it
;; throws an error.  It is not important, but I am commenting here to
;; remember I tried this.
(defun sxhkdrc-mode-restart-process ()
  "Restart the sxhkd process."
  (when-let* ((pid (shell-command-to-string "pidof sxhkd")))
    (call-process "kill" nil 0 nil "-USR1" (string-trim pid))
    t))

(declare-function notifications-notify "notifications" (&rest params))

(defun sxhkdrc-mode-restart-notify ()
  "Notify that the sxhkd process has been restarted.
Read Info node `(elisp) Desktop Notifications' for details."
  (when (featurep 'dbusbind)
    (unless (fboundp 'notifications-notify)
      (require 'notifications))
    (notifications-notify
     :title "sxhkdrc-mode"
     :body "Restarted the SXHKD process"
     :app-name "Emacs"
     :app-icon 'emacs
     :urgency 'normal
     :sound-file nil)))

(defun sxhkdrc-mode-restart ()
  "Restart the sxhkd process."
  (interactive)
  (when (sxhkdrc-mode-restart-process)
    (sxhkdrc-mode-restart-notify)))

(define-minor-mode sxhkdrc-mode-auto-restart
  "Automatically restart sxhkd after saving its file.
To set this minor mode up when opening a file that uses the
`sxhkdrc-mode', use the hook `sxhkdrc-mode-hook'."
  :global nil
  :init-value nil
  (if sxhkdrc-mode-auto-restart
      (add-hook 'after-save-hook #'sxhkdrc-mode-restart nil :local-only)
    (remove-hook 'after-save-hook #'sxhkdrc-mode-restart :local-only)))

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
