(defface fixme-and-todo-face
  '((t :foreground "#ff6c6b" :weight bold))
  "Basic face for highlighting FIXME:s and TODO:s.")

(defun add-fixme-and-todo-font-lock ()
  "Add the keywords FIXME: and TODO:."
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\):" 1 'fixme-and-todo-face t))))

(defun open-personal-wiki ()
  "Open the personal wiki."
  (interactive)
  (find-file "~/Documents/wiki.org"))

(use-package emacs
  :init
  (fido-vertical-mode t)
  (set-face-attribute 'default nil :font "IBM Plex Mono Medium" :height 130)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (electric-pair-mode t)
  (global-hl-line-mode 1)
  :hook ((prog-mode . subword-mode)
	 (prog-mode . add-fixme-and-todo-font-lock))
  :bind ([f9] . 'open-personal-wiki)
  :custom
  (gdb-many-windows t)
  (gdb-speedbar-auto-raise t)
  (treesit-language-source-alist
   '((c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
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

(use-package doom-themes
  :load-path "~/.emacs.d/local/doom-themes"
  :config (load-theme 'doom-one t)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

(use-package which-key
  :load-path "~/.emacs.d/local/which-key"
  :commands (which-key-mode)
  :init (which-key-mode))

(use-package rainbow-delimiters
  :load-path "~/.emacs.d/local/rainbow-delimiters"
  :hook (prog-mode . rainbow-delimiters-mode)
  :config (rainbow-delimiters-mode))

(use-package view
  :bind (("C-v" . View-scroll-half-page-forward)
	 ("M-v" . View-scroll-half-page-backward)))

(use-package org
  :hook ((org-mode . visual-line-mode)
	 (org-mode . flyspell-mode))
  :custom (org-startup-indented t))

(use-package ox-latex
  :custom (org-latex-compiler "xelatex")
  :config
  (add-to-list 'org-latex-classes
               '("IEEEtran" "\\documentclass{IEEEtran}"
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

(use-package markdown-mode
  :load-path "~/.emacs.d/local/markdown-mode"
  :mode "\\.md\\'"
  :magic "\\.md\\'"
  :hook ((markdown-mode . visual-line-mode)
	 (markdown-mode . flyspell-mode)))

(use-package eldoc
  :init (global-eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(defun reactivate-flymake-backend ()
  "Allow more flymake backends simultaneously (e.g. ruff for python)."
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))

(use-package eglot
  :hook ((c-ts-mode cpp-ts-mode go-ts-mode python-ts-mode js-ts-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
	      ("C-c c r" . eglot-rename)
	      ("C-c c f" . eglot-format))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-hook 'eglot-managed-mode-hook 'reactivate-flymake-backend)
  :custom-face
  (eglot-highlight-symbol-face ((t :background "#42444a"))))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
	      ("C-c ! n" . flymake-goto-next-error)
	      ("C-c ! p" . flymake-goto-prev-error)
	      ("C-c ! l" . flymake-show-buffer-diagnostics))
  :custom
  (python-flymake-command '("~/.local/bin/ruff" "check" "--quiet" "--stdin-filename" "stdin")))

(use-package c-ts-mode
  :bind (:map c-ts-mode-map
	 ("C-c C-d" . gdb)
	 ("C-c C-b" . compile)))

(use-package go-ts-mode
  :mode "\\.go\\'")

(use-package go-mod-ts-mode
  :mode "/go\\.mod\\'")

(use-package php-mode
  :load-path "~/.emacs.d/local/php-mode/lisp")

;;; .emacs ends here
