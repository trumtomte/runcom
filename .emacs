;;; Emacs config
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

(defun seb/keyword-highlight ()
  "Highlight the keywords FIXME, TODO and NOTE."
  (font-lock-add-keywords nil
			  '(("\\<\\(FIXME\\)" 0 'seb/fixme-face t)
			    ("\\<\\(TODO\\)" 0 'seb/todo-face t)
			    ("\\<\\(NOTE\\)" 0 'seb/note-face t))))

(defvar seb/selected-window nil
  "Track the selected window for mode line states.")

(defun seb/render-mode-line (left right)
  "Return a template LIST for `mode-line-format' based on LEFT and RIGHT.
This includes aligning RIGHT to the right side of the mode line."
  (let* ((right-column (- (window-width)
			 (length (format-mode-line right))))
	 (padding `((:propertize " " display (space . (:align-to ,right-column)))))
	 (mode-line (append left padding right))
	 (current-window (get-buffer-window (current-buffer))))
    (if (eq current-window seb/selected-window)
	mode-line
      (propertize (format-mode-line mode-line) 'face 'shadow))))

(defun seb/compose-mode-line (left right)
  "Set the `mode-line-format' according to LEFT and RIGHT."
  (setq seb/selected-window (selected-window))
  (add-hook 'post-command-hook (lambda () (setq seb/selected-window (selected-window))))
  (setq-default mode-line-format `((:eval (seb/render-mode-line ',left ',right)))))

(use-package emacs
  :init
  (set-face-attribute 'default nil :font "IBM Plex Mono" :height 130 :weight 'medium)
  (load-theme 'watson t)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fido-vertical-mode t)
  (electric-pair-mode t)
  (global-hl-line-mode 1)
  (repeat-mode 1)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (advice-add 'other-window :before
	      (defun seb/other-window-split-if-single (&rest _)
		"Split the frame first if there is a single window."
		(when (one-window-p) (split-window-sensibly))))

  :config
  (seb/compose-mode-line
   '(;; Buffer state
     (:eval (cond ((and buffer-file-name (buffer-modified-p))
		   (propertize " * " 'face 'warning))
		  (buffer-read-only
		   (propertize " RO " 'face 'error))
		  (t "   ")))
     ;; Buffer name
     (:propertize "%b  " face (:weight bold))
     ;; Position
     (:propertize "%l,%c  " face (:inherit font-lock-comment-face))
     ;; Errors
     (:eval (when (bound-and-true-p flymake-mode)
	      flymake-mode-line-counters)))
   ;; RIGHT
   '(;; Git branch
     (:eval (when (string= (vc-backend buffer-file-name) "Git")
	      (let ((current-branch (car (vc-git-branches))))
		(propertize (concat current-branch " ") 'face '(:inherit 'success :slant italic)))))
     ;; Major mode
     mode-name
     mode-line-process
     " "
     ;; LSP status
     (:eval (when (bound-and-true-p eglot--managed-mode)
	      (propertize "ℓ " 'face 'mode-line-emphasis)))))
  
  :hook ((prog-mode . subword-mode)
	 (prog-mode . seb/keyword-highlight)
	 (prog-mode . (lambda ()
			(setq-default truncate-lines t))))

  :bind (([f8] . 'seb/open-config)
	 ([f9] . 'seb/open-wiki)
	 ("M-\"" . 'seb/surround-region)
	 ("M-o" . 'other-window)
	 ("M-O" . 'window-swap-states)
	 ("M-S" . speedbar)
	 ("C-;" . 'mark-sexp)
	 ("C-x C-b" . 'ibuffer)
	 ("C-x C-k" . 'kill-this-buffer))
  
  :custom
  (inhibit-splash-screen t)
  (ring-bell-function 'ignore)
  (shift-select-mode nil)
  (show-paren-context-when-offscreen 'overlay)
  (speedbar-use-images nil)
  (speedbar-show-unknown-files t)
  (speedbar-directory-unshown-regexp "^\(\.\.\)$")
  (isearch-wrap-pause 'no)
  (gdb-many-windows t)
  (gdb-speedbar-auto-raise t)
  (backup-directory-alist '(("." . "~/.emacs.d/backup")))
  (auto-save-file-name-transforms '((".*" "~/.emacs.d/backup" t)))
  
  ;; Tree sitter setup
  (treesit-font-lock-level 4)
  (treesit-language-source-alist
   '((c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (js "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (python "https://github.com/tree-sitter/tree-sitter-python")))
  (major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (cpp-mode . cpp-ts-mode)
     (python-mode . python-ts-mode)
     (css-mode . css-ts-mode)
     (js-json-mode . json-ts-mode)
     (javascript-mode . js-ts-mode))))

(use-package view
  :bind (("C-v" . View-scroll-half-page-forward)
	 ("M-v" . View-scroll-half-page-backward)))

(use-package org
  :hook ((org-mode . visual-line-mode)
	 (org-mode . flyspell-mode))
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-ellipsis " ... ")
  (org-fontify-whole-heading-line t))
  
(use-package oc
  :custom (org-cite-global-bibliography '("~/Zotero/library.bib")))

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

(use-package ox-latex
  :custom (org-latex-compiler "xelatex")
  :config
  (seb/add-org-latex-class "IEEEtran")
  (seb/add-org-latex-class "acmart")
  (seb/add-org-latex-class "apa7")
  (seb/add-org-latex-class "llncs" :numbering t))

(use-package eldoc
  :init (global-eldoc-mode)
  :custom (eldoc-echo-area-use-multiline-p nil))

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

(defun seb/reactivate-flymake-backend ()
  "Allow more flymake backends simultaneously (e.g. ruff for python)."
  (declare-function eglot-flymake-backend "eglot")
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))

(use-package eglot
  :hook ((c-ts-mode cpp-ts-mode go-ts-mode python-ts-mode js-ts-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
	      ("C-c a" . eglot-code-actions)
	      ("C-c r" . eglot-rename)
	      ("C-c f" . eglot-format))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-server-programs
	       '(python-ts-mode . ("~/.local/bin/ruff" "server")))
  (add-hook 'eglot-managed-mode-hook 'seb/reactivate-flymake-backend))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
	      ("M-L" . flymake-show-buffer-diagnostics)
	      ("M-n" . flymake-goto-next-error)
	      ("M-p" . flymake-goto-prev-error))
  :custom
  ;; Python linter/formatter
  (python-flymake-command '("~/.local/bin/ruff" "check" "--quiet" "--stdin-filename" "stdin")))

(defun seb/gdb-setup-windows ()
  "Custom GDB many windows setup."
  (defvar gud-comint-buffer)
  (defvar gdb-source-window-list)
  (declare-function gdb-get-buffer-create "gdb-mi")
  (declare-function gdb-get-source-buffer "gdb-mi")
  (declare-function gdb-set-window-buffer "gdb-mi")
  (declare-function gdb-memory-buffer-name "gdb-mi")
  (declare-function gdb-locals-buffer-name "gdb-mi")
  ;; TODO: add gdb-dissassembly-buffer as well
  (gdb-get-buffer-create 'gdb-locals-values-buffer)
  (gdb-get-buffer-create 'gdb-locals-buffer)
  (gdb-get-buffer-create 'gdb-memory-buffer)
  (gdb-get-buffer-create 'gdb-breakpoints-buffer)
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

(use-package c-ts-mode
  :init (advice-add 'gdb-setup-windows :override #'seb/gdb-setup-windows)
  :bind (:map c-ts-mode-map
	      ("C-c C-g" . gdb)
	      ("C-c C-r" . gud-run)
	      ("C-c C-n" . gud-next)
	      ("C-c C-b" . gud-break)
	      ("C-c C-w" . gud-watch)
	      ("C-c C-v" . gdb-restore-windows)
	      ("C-c c" . compile))
  :hook (gud-mode . gud-tooltip-mode)
  :custom
  (c-ts-mode-indent-style 'bsd)
  (c-ts-mode-indent-offset 4))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :bind (:map go-ts-mode-map
	      ("C-c c" . compile)))

(use-package pyvenv
  :load-path "~/.emacs.d/local/pyvenv/"
  :hook (python-ts-mode . pyvenv-mode))

(use-package gptel
  :load-path "~/.emacs.d/local/gptel/"
  :custom
  (gptel-model 'llamafile)
  (gptel-use-curl nil)
  (gptel-default-mode 'org-mode)
  (gptel-backend (gptel-make-openai "llamafile"
		     :stream t
		     :protocol "http"
		     :host "127.0.0.1:8080"
		     :models '(llamafile))))
