;;; .emacs --- Personal Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A simple, and mainly standalone, Emacs configuration file.  It begins
;; with variables followed by functions and then the actual configuration.
;;
;;; Code:

(defvar seb/selected-window (selected-window)
  "Track the selected window for mode line states.")

(defun seb/open-config ()
  "Open the Emacs configuration file."
  (interactive)
  (find-file "~/.emacs"))

(defun seb/open-wiki ()
  "Open the personal wiki."
  (interactive)
  (find-file "~/Documents/wiki.org"))

(defun seb/surround-region (str)
  "Surround a region with STR."
  (interactive "sString to surround region with: ")
  (insert-pair nil str str))

(defun seb/other-window-split-if-single (&rest _)
  "Split the frame first if there is a single window."
  (when (one-window-p) (split-window-sensibly)))

(defun seb/is-selected-window ()
  "Check if this is the selected window or not."
  (eq (get-buffer-window (current-buffer)) seb/selected-window))

(defun seb/propertize-mode-line (string face)
  "Propertize STRING with FACE if the current window is selected."
  (if (seb/is-selected-window)
      (propertize string 'face face)
    (propertize string 'face 'mode-line-inactive)))

(defun seb/mode-line-buffer-state ()
  "Return the current buffer state."
   (cond ((and buffer-file-name (buffer-modified-p))
	  (seb/propertize-mode-line " * " 'warning))
	 (buffer-read-only
	  (seb/propertize-mode-line " RO " 'error))
	 (t "   ")))

(defun seb/mode-line-git-branch ()
  "Return the current Git branch name."
  (declare-function vc-git-branches "vc-git")
  (when (string= (vc-backend buffer-file-name) "Git")
    (let ((current-branch (concat (car (vc-git-branches)) " ")))
      (seb/propertize-mode-line current-branch '(:inherit warning :slant italic)))))

(defun seb/mode-line-lsp-state ()
  "Return an indicator for whether LSP is active or not."
  (when (bound-and-true-p eglot--managed-mode)
    (seb/propertize-mode-line "ℓ " 'mode-line-emphasis)))

(defun seb/mode-line-flymake-counters ()
  "Return flymake counters, propertized for the mode line."
  (defvar flymake-mode-line-counters)
  (when (bound-and-true-p flymake-mode)
    (if (seb/is-selected-window)
	flymake-mode-line-counters
      (propertize (format-mode-line flymake-mode-line-counters) 'face 'mode-line-inactive))))

(defun seb/add-org-latex-class (name &optional &key numbering)
  "Append document class NAME to `org-latex-classes'.
To disable supression of numbering set NUMBERING to true."
  (defvar org-latex-classes)
  (let ((supress-mark (if (eq numbering t) "" "*")))
    (add-to-list 'org-latex-classes
		 (list name (format "\\documentclass{%s}" name)
		       (cons "\\section{%s}" (format "\\section%s{%%s}" supress-mark))
		       (cons "\\subsection{%s}" (format "\\subsection%s{%%s}" supress-mark))
		       (cons "\\subsubsection{%s}" (format "\\subsubsection%s{%%s}" supress-mark))
		       (cons "\\paragraph{%s}" (format "\\paragraph%s{%%s}" supress-mark))))))

;; (defun seb/reactivate-flymake-backend ()
;;   "Allow more flymake backends simultaneously."
;;   (declare-function eglot-flymake-backend "eglot")
;;   (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
;;   (flymake-mode 1))

(defun seb/gdb-setup-windows ()
  "Custom GDB many windows setup."
  (defvar gud-comint-buffer)
  (defvar gdb-source-window-list)
  (declare-function gdb-get-buffer-create "gdb-mi")
  (declare-function gdb-get-source-buffer "gdb-mi")
  (declare-function gdb-set-window-buffer "gdb-mi")
  (declare-function gdb-memory-buffer-name "gdb-mi")
  (declare-function gdb-locals-buffer-name "gdb-mi")

  (gdb-get-buffer-create 'gdb-locals-values-buffer)
  (gdb-get-buffer-create 'gdb-locals-buffer)
  (gdb-get-buffer-create 'gdb-memory-buffer)
  ;; (gdb-get-buffer-create 'gdb-dissassembly-buffer)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let ((win0 (selected-window))
	(win1 (split-window-right))
	(win2 (split-window nil 40)))
    (set-window-buffer win2 (or (gdb-get-source-buffer)
				(list-buffers-noselect)))
    (setq gdb-source-window-list (list (selected-window)))
    (window-swap-states win2 win0)
    (select-window win1)
    (gdb-set-window-buffer (gdb-memory-buffer-name))
    (let ((win3 (split-window nil (/ (* (window-height) 3) 4)))
	  (win4 (split-window nil (/ (window-height) 3))))
      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win3)
      (gdb-set-window-buffer (gdb-locals-buffer-name) nil win4))
    (balance-windows)
    (select-window win0)
    (delete-window win2)))

;;; Configuration:

(use-package emacs
  :init
  (set-face-attribute 'default nil :font "IBM Plex Mono" :height 150 :weight 'medium)
  (load-theme 'sherlock t)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fringe-mode 10)
  (fido-vertical-mode t)
  (electric-pair-mode t)
  (global-hl-line-mode 1)
  (repeat-mode 1)
  (which-key-mode 1)
  (advice-add 'other-window :before #'seb/other-window-split-if-single)
  (defalias 'yes-or-no-p 'y-or-n-p)
  
  :hook ((post-command . (lambda () (setq seb/selected-window (selected-window))))
	 (prog-mode . subword-mode)
	 (prog-mode . (lambda () (setq-default truncate-lines t)))
	 ;; FIXME: these are temporary until library makers add them
	 (python-ts-mode . (lambda() (setq-local treesit-thing-settings '((python (sexp-list ""))))))
	 (go-ts-mode . (lambda() (setq-local treesit-thing-settings '((go (sexp-list ""))))))
	 (rust-ts-mode . (lambda() (setq-local treesit-thing-settings '((rust (sexp-list ""))))))
	 (css-ts-mode . (lambda() (setq-local treesit-thing-settings '((css (sexp-list "")))))))

  :bind (([f8] . 'seb/open-config)
	 ([f9] . 'seb/open-wiki)
	 ("M-\"" . 'seb/surround-region)
	 ("M-o" . 'other-window)
	 ("M-O" . 'window-swap-states)
	 ("M-S" . speedbar)
	 ("M-T" . eshell)
	 ("M-<up>" . 'View-scroll-half-page-backward)
	 ("M-<down>" . 'View-scroll-half-page-forward)
	 ("M-[ M-s" . 'save-buffer)
	 ("M-[ M-f" . 'find-file)
	 ("M-[ M-d" . 'dired)
	 ("M-[ M-b" . 'ibuffer)
	 ("M-[ M-g" . 'imenu)
	 ("M-[ M-<SPC>" . 'switch-to-buffer)
	 ("C-;" . 'mark-sexp)
	 ("C-x C-b" . 'ibuffer))

  :custom
  (mode-line-format
   '((:eval (seb/mode-line-buffer-state))
     (:propertize "%b  " face bold)
     (:propertize "%l,%c  " face shadow)
     (:eval (seb/mode-line-flymake-counters))
     mode-line-format-right-align
     (:eval (seb/mode-line-git-branch))
     mode-name
     mode-line-process
     " "
     (:eval (seb/mode-line-lsp-state))))
  
  (backup-directory-alist '(("." . "~/.emacs.d/backup")))
  (auto-save-file-name-transforms '((".*" "~/.emacs.d/backup" t)))
  (inhibit-splash-screen t)
  (ring-bell-function 'ignore)
  (shift-select-mode nil)
  (show-paren-context-when-offscreen 'overlay)
  (isearch-wrap-pause 'no)
  (imenu-auto-rescan t)
  (diff-font-lock-syntax nil)
  (speedbar-use-images nil)
  (speedbar-show-unknown-files t)
  (speedbar-directory-unshown-regexp "^\(\.\.\)$")
  
  ;; NOTE: remap major mode to treesitter equivalents
  (major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (cpp-mode . cpp-ts-mode)
     (python-mode . python-ts-mode)
     (css-mode . css-ts-mode)
     (js-json-mode . json-ts-mode)
     (javascript-mode . js-ts-mode))))

(use-package view
  :bind (("C-v" . View-scroll-half-page-forward)
	 ("M-v" . View-scroll-half-page-backward))
  :custom (scroll-preserve-screen-position 'always))

(use-package org
  :hook ((org-mode . visual-line-mode)
	 (org-mode . flyspell-mode))
  :bind (("M-[ M-2" . org-cite-insert)
	 ("M-[ M-1" . org-emphasize))
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-ellipsis " ... "))
  
(use-package oc
  :custom (org-cite-global-bibliography '("~/Zotero/library.bib")))

(use-package ox-latex
  :custom (org-latex-compiler "xelatex")
  :config
  (seb/add-org-latex-class "IEEEtran")
  (seb/add-org-latex-class "acmart")
  (seb/add-org-latex-class "apa7")
  (seb/add-org-latex-class "llncs" :numbering t)
  (add-to-list 'org-latex-classes
	       '("thesis" "\\documentclass[11pt]{report}"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(use-package eldoc
  :custom
  (echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer 'maybe))

(use-package eglot
  :hook ((c-ts-mode . eglot-ensure)
	 (cpp-ts-mode . eglot-ensure)
	 (go-ts-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure)
	 (js-ts-mode . eglot-ensure)
	 (typescript-ts-mode . eglot-ensure)
	 (rust-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map
	      ("C-c a" . eglot-code-actions)
	      ("C-c r" . eglot-rename)
	      ("C-c f" . eglot-format))
  :config
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  ;; (add-hook 'eglot-managed-mode-hook 'seb/reactivate-flymake-backend)
  (add-to-list 'eglot-server-programs
	       '(python-ts-mode . ("~/.local/bin/ruff" "server")))
  (add-to-list 'eglot-server-programs
	       '((js-ts-mode typescript-ts-mode) . ("~/.deno/bin/deno" "lsp"))))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
	      ("M-n" . flymake-goto-next-error)
	      ("M-p" . flymake-goto-prev-error)
	      ("M-D" . flymake-show-buffer-diagnostics)))

(use-package c-ts-mode
  :functions (c-ts-mode-toggle-comment-style)
  :init (advice-add 'gdb-setup-windows :override #'seb/gdb-setup-windows)
  :bind (:map c-ts-mode-map
	      ("C-c C-g" . gdb)
	      ("C-c C-r" . gud-run)
	      ("C-c C-n" . gud-next)
	      ("C-c C-b" . gud-break)
	      ("C-c C-w" . gud-watch)
	      ("C-c C-v" . gdb-restore-windows)
	      ("M-[ M-c" . compile))
  :hook ((gud-mode . gud-tooltip-mode)
	 (c-ts-mode . (lambda () (c-ts-mode-toggle-comment-style -1))))
  :custom
  (c-ts-mode-indent-style 'bsd)
  (c-ts-mode-indent-offset 4)
  (gdb-many-windows t)
  (gdb-speedbar-auto-raise t))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :bind (:map go-ts-mode-map
	      ("M-[ M-c" . compile)))

(use-package pyvenv
  :load-path "~/.emacs.d/local/pyvenv/"
  :hook (python-ts-mode . pyvenv-mode))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :custom (js-indent-level 2))

(use-package js-ts-mode
  :mode "\\.js\\'"
  :custom (js-indent-level 2))

(use-package web-mode
  :load-path "~/.emacs.d/local/web-mode/"
  :mode "\\.html\\'")

(use-package rmail
  :custom
  (user-full-name "Sebastian Bengtegård")
  (user-mail-address "sebastianbengtegard@pm.me")
  (rmail-primary-inbox-list '("imap://sebastianbengtegard%40pm.me@127.0.0.1:1143"))
  (rmail-remote-password-required t)
  (rmail-preserve-inbox t)
  (rmail-file-name "~/rmail.mbox")
  (rmail-mime-prefer-html nil)
  (rmail-mime-render-html-function 'rmail-mime-render-html-lynx)
  (rmail-display-summary t)
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-smtp-server "127.0.0.1")
  (smtpmail-smtp-service 1025))

;;; .emacs ends here
