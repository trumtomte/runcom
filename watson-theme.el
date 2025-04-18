;;; watson-theme.el --- Personal Emacs theme -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A simple Emacs theme inspired by Doom Emacs / Atom One Dark.
;;
;;; Code:

(deftheme watson
  "A simple Emacs theme inspired by Doom Emacs / Atom One Dark.")

(let ((main-bg  "#282c34")
      (main-fg	"#bbc2cf")
      (alt-bg   "#21242b")
      (red      "#ff6c6b")
      (red-bg   "#53383f")
      (green    "#98be65")
      (green-bg "#3e493d")
      (blue     "#51afef")
      (blue-bg  "#304659")
      (orange   "#da8548")
      (yellow   "#ecbe7b")
      (magenta  "#c678dd")
      (pink     "#dcaeea")
      (violet   "#a9a1e1")
      (cyan     "#4db6bd")
      (gray-1   "#1B2229")
      (gray-2   "#23272e")
      (gray-3   "#3f444a")
      (gray-4   "#5B6268")
      (gray-5   "#73797e")
      (gray-6   "#dfdfdf"))

  (custom-theme-set-faces
   'watson
   
   ;; Basics
   `(default					((t :background ,main-bg :foreground ,main-fg)))
   `(italic					((t :slant italic)))
   `(fringe					((t :foreground ,green)))
   `(vertical-border				((t :foreground ,gray-1)))
   `(region					((t :background ,gray-3 :distant-foreground ,gray-5 :extend t)))
   `(cursor					((t :background ,blue)))
   `(shadow					((t :foreground ,gray-4)))
   `(minibuffer-prompt				((t :foreground ,blue)))
   `(tooltip					((t :background ,alt-bg :foreground ,main-fg)))
   `(secondary-selection			((t :background ,gray-3 :extend t)))
   `(highlight					((t :background ,blue :foreground ,gray-1 :distant-foreground ,gray-6)))
   `(lazy-highlight				((t :background ,green-bg :foreground ,green :distant-foreground ,gray-1 :weight bold)))
   `(match					((t :foreground ,green :background ,gray-1 :weight bold)))
   `(link					((t :foreground ,blue :underline t :weight bold)))
   `(link-visited				((t :inherit link :foreground ,violet)))
   `(error					((t :foreground ,red)))
   `(warning					((t :foreground ,yellow)))
   `(success					((t :foreground ,green)))
   `(hl-line					((t :background ,alt-bg :extend t)))
   `(trailing-whitespace			((t :background ,red)))
   `(escape-glyph				((t :foreground ,cyan)))
   `(nobreak-space				((t :inherit escape-glyph :underline t)))

   ;; Font lock
   `(font-lock-builtin-face			((t :foreground ,magenta)))
   `(font-lock-comment-face			((t :foreground ,gray-4)))
   `(font-lock-comment-delimiter-face		((t :inherit font-lock-comment-face)))
   `(font-lock-doc-face				((t :foreground ,gray-5)))
   `(font-lock-constant-face			((t :foreground ,violet)))
   `(font-lock-function-name-face		((t :foreground ,magenta)))
   `(font-lock-keyword-face			((t :foreground ,blue)))
   `(font-lock-string-face			((t :foreground ,green)))
   `(font-lock-type-face			((t :foreground ,yellow)))
   `(font-lock-variable-name-face		((t :foreground ,pink)))
   `(font-lock-number-face			((t :foreground ,orange)))
   `(font-lock-warning-face			((t :inherit warning)))
   `(font-lock-negation-char-face		((t :foreground ,blue :weight bold)))
   `(font-lock-preprocessor-face		((t :foreground ,blue :weight bold)))
   `(font-lock-preprocessor-char-face		((t :foreground ,blue :weight bold)))
   `(font-lock-regexp-grouping-backslash	((t :foreground ,blue :weight bold)))
   `(font-lock-regexp-grouping-construct	((t :foreground ,blue :weight bold)))
   `(eglot-highlight-symbol-face		((t :underline t)))
   
   ;; Mode line
   `(mode-line					((t :background ,gray-1 :foreground ,main-fg :box (:line-width 4 :color ,gray-1))))
   `(mode-line-active				((t :inherit mode-line)))
   `(mode-line-inactive				((t :background ,alt-bg :foreground ,gray-4 :box (:line-width 4 :color ,alt-bg))))
   `(mode-line-emphasis				((t :foreground ,blue)))
   `(mode-line-highlight			((t :inherit highlight :box (:line-width 4 :color ,blue))))
   `(mode-line-buffer-id			((t :weight bold)))
   `(mode-line-git-branch			((t :foreground ,green :slant italic)))
   `(header-line				((t :background ,alt-bg :foreground ,main-fg :box (:line-width 4 :color ,alt-bg))))
   
   ;; Search & Completion
   `(icomplete-selected-match			((t :foreground ,main-fg :background ,gray-1 :box (:line-width (4 . 2) :color ,gray-1))))
   `(icomplete-first-match			((t :weight regular :foreground ,yellow)))
   `(completions-common-part			((t :foreground ,blue)))
   `(completions-first-difference		((t :foreground ,yellow)))
   `(completion-preview-exact			((t :inherit completion-preview-common)))
   `(isearch					((t :inherit lazy-highlight :weight bold)))
   `(isearch-fail				((t :background ,red :foreground ,gray-1 :weight bold)))
   
   ;; Bracket matching
   `(show-paren-match				((t :foreground ,blue :background ,blue-bg :weight bold)))
   `(show-paren-mismatch			((t :foreground ,red :background ,red-bg :weight bold)))
   
   ;; Flymake
   `(flymake-error				((t :underline (:style wave :color ,red))))
   `(flymake-note				((t :underline (:style wave :color ,green))))
   `(flymake-warning				((t :underline (:style wave :color ,orange))))
   `(flyspell-incorrect 			((t :underline (:style wave :color ,red) :inherit nil)))
   `(flyspell-duplicate 			((t :underline (:style wave :color ,yellow) :inherit nil)))
   
   ;; UI
   `(custom-button				((t :foreground ,blue :background ,main-bg :box (:line-width 1))))
   `(custom-button-unraised			((t :foreground ,violet :background ,main-bg :box (:line-width 1))))
   `(custom-button-pressed-unraised		((t :foreground ,main-bg :background ,violet :box (:line-width 1))))
   `(custom-button-pressed			((t :foreground ,main-bg :background ,blue :box (:line-width 1))))
   `(custom-button-mouse			((t :foreground ,main-bg :background ,blue :box (:line-width 1))))
   `(custom-variable-button			((t :foreground ,green :underline t)))
   `(custom-saved				((t :foreground ,green :background ,green-bg :bold t)))
   `(custom-comment				((t :foreground ,main-fg :background ,gray-3)))
   `(custom-comment-tag				((t :foreground ,gray-3)))
   `(custom-modified				((t :foreground ,blue :background ,blue-bg)))
   `(custom-variable-tag			((t :foreground ,magenta)))
   `(custom-visibility				((t :foreground ,blue)))
   `(custom-group-subtitle			((t :foreground ,red)))
   `(custom-group-tag				((t :foreground ,violet)))
   `(custom-group-tag-1				((t :foreground ,blue)))
   `(custom-set					((t :foreground ,yellow :background ,main-bg)))
   `(custom-themed				((t :foreground ,yellow :background ,main-bg)))
   `(custom-invalid				((t :foreground ,red :background ,red-bg)))
   `(custom-variable-obsolete			((t :foreground ,gray-3 :background ,main-bg)))
   `(custom-state				((t :foreground ,green :background ,green-bg)))
   `(custom-changed				((t :foreground ,blue :background ,main-bg)))
   `(widget-field				((t :background ,gray-1 :extend t :box (:line-width 1 :color ,gray-1))))
   `(widget-single-line-field			((t :background ,gray-1)))
   `(Info-quoted				((t :foreground ,violet)))
   `(help-key-binding				((t :foreground ,yellow :background ,gray-2 :box (:line-width 1 :color ,gray-3))))
   `(tutorial-warning-face			((t :foreground ,orange)))
   `(info-menu-star				((t :foreground ,pink)))
   
   ;; Dired
   `(dired-directory				((t :foreground ,blue)))
   `(dired-ignored				((t :foreground ,gray-4)))
   `(dired-flagged				((t :foreground ,red)))
   `(dired-header				((t :foreground ,blue :weight bold)))
   `(dired-mark					((t :foreground ,green)))
   `(dired-marked				((t :foreground ,yellow :weight bold)))
   `(dired-perm-write				((t :foreground ,main-fg :underline t)))
   `(dired-symlink				((t :inherit shadow)))
   `(dired-broken-symlink			((t :inherit show-paren-mismatch)))
   `(dired-warning				((t :inherit warning)))

   ;; Eshell
   `(eshell-prompt				((t :foreground ,yellow :weight bold)))
   `(eshell-ls-archive				((t :foreground ,magenta)))
   `(eshell-ls-clutter				((t :foreground ,red)))
   `(eshell-ls-directory			((t :foreground ,blue)))
   `(eshell-ls-executable			((t :foreground ,green)))
   `(eshell-ls-missing				((t :foreground ,orange)))
   `(eshell-ls-product				((t :foreground ,orange)))
   `(eshell-ls-readonly				((t :foreground ,red)))
   `(eshell-ls-special				((t :foreground ,violet)))
   `(eshell-ls-backup				((t :inherit shadow)))
   `(eshell-ls-symlink				((t :inherit shadow)))
   `(eshell-ls-unreadable			((t :inherit shadow)))

   ;; Speedbar
   `(speedbar-button-face			((t :inherit shadow)))
   `(speedbar-directory-face			((t :inherit dired-directory)))
   `(speedbar-file-face				((t :inherit default)))
   `(speedbar-highlight-face			((t :inherit highlight)))
   `(speedbar-selected-face			((t :inherit dired-marked)))
   `(speedbar-tag-face				((t :foreground ,yellow)))

   ;; Diff
   `(diff-hl-change				((t :foreground ,orange :background ,orange)))
   `(diff-hl-delete				((t :foreground ,red :background ,red)))
   `(diff-hl-insert				((t :foreground ,green :background ,green)))
   `(diff-added					((t :inherit hl-line :foreground ,green)))
   `(diff-changed				((t :foreground ,violet)))
   `(diff-removed				((t :foreground ,red :background ,gray-2)))
   `(diff-header				((t :foreground ,cyan)))
   `(diff-file-header				((t :foreground ,blue)))
   `(diff-hunk-header				((t :foreground ,violet)))
   `(diff-refine-added				((t :inherit diff-added :inverse-video t)))
   `(diff-refine-changed			((t :inherit diff-changed :inverse-video t)))
   `(diff-refine-removed			((t :inherit diff-removed :inverse-video t)))

   ;; Compilation
   `(compilation-column-number			((t :inherit font-lock-comment-face)))
   `(compilation-line-number			((t :foreground ,blue)))
   `(compilation-error				((t :inherit error :weight bold)))
   `(compilation-warning			((t :inherit warning :slant italic)))
   `(compilation-info				((t :inherit success)))
   `(compilation-mode-line-exit 		((t :inherit compilation-info)))
   `(compilation-mode-line-fail 		((t :inherit compilation-error)))

   ;; TeX errors
   `(TeX-error-description-error		((t :inherit error :weight bold)))
   `(TeX-error-description-warning		((t :inherit warning :slant italic)))
   `(TeX-error-description-tex-said		((t :inherit success)))

   ;; Outline (org headers etc.)
   `(outline-1 					((t :foreground ,blue :weight bold :extend t)))
   `(outline-2 					((t :foreground ,magenta :weight bold :extend t)))
   `(outline-3 					((t :foreground ,violet :weight bold :extend t)))
   `(outline-4 					((t :foreground ,pink :weight bold :extend t)))
   `(outline-5 					((t :foreground ,yellow :weight bold :extend t)))
   `(outline-6 					((t :foreground ,green :weight bold :extend t)))
   `(outline-7 					((t :foreground ,orange :weight bold :extend t)))
   `(outline-8 					((t :foreground ,cyan :weight bold :extend t)))

   ;; Org
   `(org-document-info				((t :foreground ,magenta)))
   `(org-document-title				((t :foreground ,yellow :weight bold)))
   `(org-date					((t :foreground ,yellow)))
   `(org-block					((t :background ,gray-2 :extend t :box (:line-width (10 . -1) :color ,gray-2))))
   `(org-block-background			((t :background ,gray-2 :extend t)))
   `(org-block-begin-line			((t :inherit org-block :foreground ,gray-5 :box (:line-width (10 . 4) :color ,gray-2))))
   `(org-block-end-line				((t :inherit org-block-begin-line)))
   `(org-code					((t :inherit org-block :foreground ,orange)))
   `(org-verbatim				((t :foreground ,green)))
   `(org-warning				((t :foreground ,yellow)))
   `(org-quote					((t :inherit org-block :slant italic)))
   `(org-cite					((t :foreground ,cyan)))
   `(org-cite-key				((t :foreground ,orange)))
   `(org-todo					((t :foreground ,yellow :weight bold)))
   `(org-headline-done				((t :foreground ,gray-4)))
   `(org-done					((t :inherit org-headline-done :weight bold)))
   `(org-priority				((t :foreground ,red)))
   `(org-checkbox				((t :inherit org-todo)))
   `(org-checkbox-statistics-done		((t :inherit org-done)))
   `(org-checkbox-statistics-todo		((t :inherit org-todo)))
   `(org-drawer					((t :inherit shadow)))
   `(org-ellipsis				((t :foreground ,gray-5)))
   `(org-table					((t :foreground ,pink :background ,gray-2 :box (:line-width (10 . -1) :color ,gray-2))))
   `(org-formula				((t :foreground ,cyan)))
   `(org-footnote				((t :foreground ,orange)))
   `(org-hide					((t :foreground ,main-bg)))
   `(org-latex-and-related			((t :foreground ,gray-6 :weight bold)))
   `(org-link					((t :inherit link)))
   `(org-list-dt				((t :foreground ,yellow)))
   `(org-meta-line				((t :inherit shadow)))
   `(org-property-value				((t :inherit shadow)))
   `(org-special-keyword			((t :inherit shadow)))
   `(org-tag					((t :inherit shadow)))
   `(org-archived				((t :inherit shadow)))

   ;; Rmail etc.
   `(message-cited-text				((t :foreground ,violet)))
   `(message-cited-text-1			((t :foreground ,yellow)))
   `(message-cited-text-2			((t :foreground ,magenta)))
   `(message-cited-text-3			((t :foreground ,green)))
   `(message-cited-text-4			((t :foreground ,green)))
   `(message-header-name			((t :foreground ,magenta)))
   `(message-header-to				((t :foreground ,green)))
   `(message-header-cc				((t :foreground ,green)))
   `(message-header-subject			((t :foreground ,yellow)))
   `(message-header-other			((t :foreground ,blue)))
   `(message-separator				((t :foreground ,gray-4)))
   `(message-signature-separator		((t :foreground ,gray-4))))

  (add-hook 'prog-mode-hook
	    (lambda ()
	      (font-lock-add-keywords nil
				      '(("\\<\\(FIXME\\)" 1 'error prepend)
					("\\<\\(TODO\\)" 1 'warning prepend)
					("\\<\\(NOTE\\)" 1 'success prepend))))))

(provide-theme 'watson)
(provide 'watson-theme)

;;; watson-theme.el ends here
