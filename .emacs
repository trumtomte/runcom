;;; package --- Sebbes Emacs configuration
;;; Commentary:
;;; Code:

;;; TODO: personal color theme (i.e. replace the doom theme)
;;;       list-faces-display
;;; blue: #51afef, #6bbbf1
;;; cyan: #46d9ff, #61deff
;;; green: #98be65, #a7c77c
;;; white: #bbc2cf, #dfdfdf
;;; black: #282c34 (this is the editor bg), fg #1b2229, bg, #202328 (bright)
;;; magenta: #c678dd, #ce8ce2
;;; yellow: #ecbe7b, #eec78e
;;; red: #ff6c6b, #ff8281
;;; orange: #da8548
;;; purple: #a9a1e1
;;; pink: #dcaeea

(defun seb/keyword-highlight ()
  "Highlight the keywords `FIXME:`, `TODO:` and `NOTE:`."
  (font-lock-add-keywords nil
			  '(("\\<\\(FIXME\\):" 1 '((t :foreground "#ff6c6b" :weight bold :underline t)) t)
			    ("\\<\\(TODO\\):" 1 '((t :foreground "#ecbe7b" :weight bold :underline t)) t)
			    ("\\<\\(NOTE\\):" 1 '((t :foreground "#98be65" :weight bold :underline t)) t))))
				 
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

(use-package emacs
  :init
  (set-face-attribute 'default nil :font "IBM Plex Mono Medium" :height 130)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fido-vertical-mode t)
  (electric-pair-mode t)
  (global-hl-line-mode 1)
  (repeat-mode 1)
    
  (advice-add 'other-window :before
	      (defun seb/other-window-split-if-single (&rest _)
		"Split the frame first if there is a single window."
		(when (one-window-p) (split-window-sensibly))))
  
  :hook ((prog-mode . subword-mode)
	 (prog-mode . seb/keyword-highlight)
	 (prog-mode . (lambda ()
			(setq-default truncate-lines t))))
  
  :bind (([f8] . 'seb/open-config)
	 ([f9] . 'seb/open-wiki)
	 ("M-\"" . 'seb/surround-region)
	 ("M-o" . 'other-window)
	 ("M-O" . 'window-swap-states)
	 ("C-x C-b" . 'ibuffer)
	 ("C-x C-k" . 'kill-this-buffer)
	 ("C-<return>" . 'switch-to-buffer)) ;; NOTE: maybe project- or find-file?
  
  :custom
  (ring-bell-function 'ignore)
  (shift-select-mode nil)
  (show-paren-context-when-offscreen 'overlay)
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
     (javascript-mode . js-ts-mode)))
  :custom-face
  (lazy-highlight ((t (:background "#3e493d" :foreground "#98be65" :weight bold)))))

(use-package doom-themes
  :load-path "~/.emacs.d/local/doom-themes"
  :config (load-theme 'doom-one t)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

;; (use-package which-key
;;   :load-path "~/.emacs.d/local/which-key"
;;   :commands (which-key-mode)
;;   :init (which-key-mode))

(use-package view
  :bind (("C-v" . View-scroll-half-page-forward)
	 ("M-v" . View-scroll-half-page-backward)))

(use-package org
  :hook ((org-mode . visual-line-mode)
	 (org-mode . flyspell-mode))
  :custom (org-startup-indented t))
  
(use-package oc
  ;; NOTE: use M-j to exit fido when searching for citations
  :custom
  (org-cite-global-bibliography '("/home/sebbe/Zotero/library.bib")))

(use-package ox-latex
  :custom
  (org-latex-compiler "xelatex")
  :config
  ; TODO: test whether we can write these as the one above/below
  (add-to-list 'org-latex-classes
               '("IEEEtran" "\\documentclass{IEEEtran}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("acmart" "\\documentclass{acmart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("llncs" "\\documentclass{llncs}"
                 ("\\section{%s}" . "\\section{%s}")
                 ("\\subsection{%s}" . "\\subsection{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                 ("\\paragraph{%s}" . "\\paragraph{%s}")))
  (add-to-list 'org-latex-classes
	       '("apa7" "\\documentclass{apa7}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}"))))

;; (use-package markdown-mode
;;   :load-path "~/.emacs.d/local/markdown-mode"
;;   :mode "\\.md\\'"
;;   :magic "\\.md\\'"
;;   :hook ((markdown-mode . visual-line-mode)
;; 	 (markdown-mode . flyspell-mode)))

(use-package eldoc
  :init (global-eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(defun seb/reactivate-flymake-backend ()
  "Allow more flymake backends simultaneously (e.g. ruff for python)."
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
  (add-hook 'eglot-managed-mode-hook 'seb/reactivate-flymake-backend)
  :custom-face
  (eglot-highlight-symbol-face ((t :underline t))))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
	      ("M-n" . flymake-goto-next-error)
	      ("M-p" . flymake-goto-prev-error))
  :custom
  (python-flymake-command '("~/.local/bin/ruff" "check" "--quiet" "--stdin-filename" "stdin")))

(defun seb/gdb-setup-windows ()
  "Custom GDB many windows setup."
  ;; ignore compiler warnings
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
    (gdb-set-window-buffer (gdb-locals-buffer-name))
    (let ((win3 (split-window nil ( / ( * (window-height) 3) 4)))
	  (win4 (split-window nil ( / (window-height) 3))))
      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win3)
      (gdb-set-window-buffer (gdb-memory-buffer-name) nil win4))
    (select-window win0)))

(use-package c-ts-mode
  :init
  (advice-add 'gdb-setup-windows :override #'seb/gdb-setup-windows)
  :bind (:map c-ts-mode-map
	      ("C-c d" . gdb)
	      ("C-c c" . compile))
  :custom
  (c-ts-mode-indent-style 'bsd)
  (c-ts-mode-indent-offset 4))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :bind (:map go-ts-mode-map
	      ("C-c c" . compile)))

;; (use-package go-mod-ts-mode
;;   :mode "/go\\.mod\\'")

;; (use-package php-mode
;;   :load-path "~/.emacs.d/local/php-mode/lisp"
;;   :mode "\\.php\\'")

(defun set-personal-gnus-topics ()
  "Set custom topics for topic mode."
  (setq gnus-topic-topology '(("Gnus" visible)
			      (("protonmail" visible nil nil))
			      (("doris" visible nil nil))))
  (setq gnus-topic-alist '(("protonmail"
                            "nnimap+protonmail:INBOX"
                            "nnimap+protonmail:Sent"
                            "nnimap+protonmail:Archive"
                            "nnimap+protonmail:Trash"
                            "nnimap+protonmail:All Mail"
			    "nnimap+protonmail:Drafts"
			    "nnimap+protonmail:Starred"
			    "nnimap+protonmail:Spam")
			   ("doris"
                            "nnimap+doris:INBOX"
                            "nnimap+doris:INBOX.Sent"
                            "nnimap+doris:INBOX.Archive"
                            "nnimap+doris:INBOX.Trash"
			    "nnimap+doris:INBOX.Drafts"
			    "nnimap+doris:INBOX.spam")
                           ("Gnus")))
  (gnus-topic-set-parameters "protonmail" '((display . 100)))
  (gnus-topic-set-parameters "doris" '((display . 100)))
  ;; (gnus-summary-toggle-threads -1)
  )

(use-package gnus
  ;; :hook (
	 ;; (gnus-group-mode . gnus-topic-mode)
	 ;; (gnus-topic-mode . set-personal-gnus-topics)
  ;; 	 )
  :custom
  (user-full-name "Sebastian Bengtegård")
  (user-mail-address "sebastianbengtegard@protonmail.com")
  
  (gnus-select-method '(nnnil nil))
  (gnus-secondary-select-methods
   '((nnimap "protonmail"
             (nnimap-address "127.0.0.1")
             (nnimap-server-port 1143)
             (nnimap-stream starttls)
	     (nnimap-inbox "INBOX")
	     (nnimap-expiry-wait never))
     (nnimap "doris"
	     (nnimap-address "csrv11.aname.net")
	     (nnimap-server-port 993)
	     (nnimap-stream ssl)
	     (nnimap-inbox "INBOX")
	     (nnimap-expiry-wait never))))
  
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-smtp-server "127.0.0.1")
  (smtpmail-smtp-service 1025)
  
  (gnus-use-dribble-file nil)
  (gnus-use-cache t)
  (gnus-summary-line-format "%U  %~(max-right 4)o-%~(cut-left 4)~(max-right 2)o-%~(cut-left 6)~(max-right 2)o   %-25,25n   %B%s\n")
  ;; (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date (not gnus-thread-sort-by-number)))
  (mm-text-html-renderer 'gnus-w3m))
