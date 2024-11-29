(deftheme watson
  "Inspired by Doom One / Atom One Dark.")

(defface seb/todo-face
  '((t :inherit warning :underline t))
  "Face for TODOs in comments.")

(defface seb/fixme-face
  '((t :inherit error :underline t))
  "Face for FIXMEs in comments.")

(defface seb/note-face
  '((t :inherit success :underline t))
  "Face for NOTEs in comments.")

(let ((bg         "#282c34")
      (fg         "#bbc2cf")
      (bg-alt     "#21242b")
      (base0      "#1B2229") 
      (base2      "#202328") ; NOTE: could possibly merge base2 and base3
      (base3      "#23272e") 
      (base4      "#3f444a") 
      (base5      "#5B6268") 
      (base6      "#73797e") 
      (base8      "#DFDFDF") 
      (red        "#ff6c6b")
      (red-bg     "#53383f")
      (orange     "#da8548")
      (yellow     "#ECBE7B") 
      (green      "#98be65")
      (green-bg   "#3e493d")
      (blue       "#51afef")
      (blue-bg    "#304659")
      (teal       "#4db5bd")
      (cyan       "#46D9FF")
      (magenta    "#c678dd")
      (pink       "#dcaeea")
      (violet     "#a9a1e1"))

  (custom-theme-set-faces
   'watson
   `(italic ((t (:slant italic :underline nil))))
   `(default ((t (:background ,bg :foreground ,fg))))
   `(escape-glyph ((t (:foreground ,cyan))))
   `(fringe ((t (:inherit default :foreground ,green))))
   `(region ((t (:background ,base4 :distant-foreground ,base6 :extend t))))
   `(cursor ((t (:background ,blue))))
   `(shadow ((t (:foreground ,base5))))
   `(minibuffer-prompt ((t (:foreground ,blue))))
   `(tooltip ((t (:background ,bg-alt :foreground ,fg))))
   `(secondary-selection ((t (:background ,base4 :extend t))))
   `(highlight ((t (:background ,blue :foreground ,base0 :distant-foreground ,base8))))
   `(lazy-highlight ((t (:background ,green-bg :distant-foreground ,base0 :foreground ,green :weight bold))))
   `(match ((t (:foreground ,green :background ,base0 :weight bold))))
   `(trailing-whitespace ((t (:background ,red))))
   `(nobreak-space ((t (:inherit escape-glyph :underline t))))
   `(vertical-border ((t (:background ,base0 :foreground ,base0))))
   `(link ((t (:foreground ,blue :underline t :weight bold))))
   `(error ((t (:foreground ,red))))
   `(warning ((t (:foreground ,yellow))))
   `(success ((t (:foreground ,green))))
   `(hl-line ((t (:background ,bg-alt :extend t))))
   
   `(mode-line ((t (:background ,base0 :foreground ,fg :box (:line-width 4 :color ,base0 :style flat)))))
   `(mode-line-active ((t (:inherit mode-line))))
   `(mode-line-inactive ((t (:background ,bg-alt :foreground ,base4 :box (:line-width 4 :color ,bg-alt :style flat)))))
   `(mode-line-emphasis ((t (:foreground ,blue))))
   `(mode-line-highlight ((t (:inherit highlight :box (:line-width 4 :color ,blue :style flat)))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(header-line ((t (:background ,bg-alt :foreground ,fg :box (:line-width 4 :color ,bg-alt :style flat)))))

   `(icomplete-selected-match ((t (:foreground ,fg :background ,base0 :box (:line-width (4 . 2) :color ,base0 :style flat)))))
   `(icomplete-first-match ((t (:weight regular :foreground ,yellow))))
   `(completions-common-part ((t (:foreground, blue))))
   `(completions-first-difference ((t (:foreground, yellow))))

   `(isearch ((t (:inherit lazy-highlight :weight bold))))
   `(isearch-fail ((t (:background ,red :foreground ,base0 :weight bold))))

   `(show-paren-match ((t (:foreground ,red :background ,base0 :weight ultra-bold))))
   `(show-paren-mismatch ((t (:foreground ,base0 :background ,red :weight ultra-bold))))
   
   `(font-lock-builtin-face ((t (:foreground ,magenta))))
   `(font-lock-comment-face ((t (:foreground ,base5 :background unspecified))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground ,base6))))
   `(font-lock-constant-face ((t (:foreground ,violet))))
   `(font-lock-function-name-face ((t (:foreground ,magenta))))
   `(font-lock-keyword-face ((t (:foreground ,blue))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,pink))))
   `(font-lock-number-face ((t (:foreground ,orange))))
   `(font-lock-warning-face ((t (:inherit warning))))
   `(font-lock-negation-char-face ((t (:inherit bold :foreground ,blue))))
   `(font-lock-preprocessor-face ((t (:inherit bold :foreground ,blue))))
   `(font-lock-preprocessor-char-face ((t (:inherit bold :foreground ,blue))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit bold :foreground ,blue))))
   `(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground ,blue))))
   
   `(eglot-highlight-symbol-face ((t :underline t)))

   `(flymake-error ((t (:underline (:style wave :color ,red)))))
   `(flymake-note ((t (:underline (:style wave :color ,green)))))
   `(flymake-warning ((t (:underline (:style wave :color ,orange)))))
   `(flyspell-incorrect ((t (:underline (:style wave :color ,red) :inherit unspecified))))
   `(flyspell-duplicate ((t (:underline (:style wave :color ,yellow) :inherit unspecified))))

   `(custom-button ((t (:foreground ,blue :background ,bg :box (:line-width 1 :style nil)))))
   `(custom-button-unraised ((t (:foreground ,violet :background ,bg :box (:line-width 1 :style nil)))))
   `(custom-button-pressed-unraised ((t (:foreground ,bg :background ,violet :box (:line-width 1 :style nil)))))
   `(custom-button-pressed ((t (:foreground ,bg :background ,blue :box (:line-width 1 :style nil)))))
   `(custom-button-mouse ((t (:foreground ,bg :background ,blue :box (:line-width 1 :style nil)))))
   `(custom-variable-button ((t (:foreground ,green :underline t))))
   `(custom-saved ((t (:foreground ,green :background ,green-bg :bold t))))
   `(custom-comment ((t (:foreground ,fg :background ,base4))))
   `(custom-comment-tag ((t (:foreground ,base4))))
   `(custom-modified ((t (:foreground ,blue :background ,blue-bg))))
   `(custom-variable-tag ((t (:foreground ,magenta))))
   `(custom-visibility ((t (:foreground ,blue :underline unspecified))))
   `(custom-group-subtitle ((t (:foreground ,red))))
   `(custom-group-tag ((t (:foreground ,violet))))
   `(custom-group-tag-1 ((t (:foreground ,blue))))
   `(custom-set ((t (:foreground ,yellow :background ,bg))))
   `(custom-themed ((t (:foreground ,yellow :background ,bg))))
   `(custom-invalid ((t (:foreground ,red :background ,red-bg))))
   `(custom-variable-obsolete ((t (:foreground ,base4 :background ,bg))))
   `(custom-state ((t (:foreground ,green :background ,green-bg))))
   `(custom-changed ((t (:foreground ,blue :background ,bg))))
   `(widget-field ((t (:extend t :background ,base0 :box (:line-width 1 :color ,base0)))))
   `(widget-single-line-field ((t (:background ,base0))))

   `(dired-directory ((t (:foreground ,magenta))))
   `(dired-ignored ((t (:foreground ,base5))))
   `(dired-flagged ((t (:foreground ,red))))
   `(dired-header ((t (:foreground ,blue :weight bold))))
   `(dired-mark ((t (:foreground ,orange :weight bold))))
   `(dired-marked ((t (:foreground ,magenta :weight bold :inverse-video t))))
   `(dired-perm-write ((t (:foreground ,fg :underline t))))
   `(dired-symlink ((t (:foreground ,cyan :weight bold))))
   `(dired-warning ((t (:foreground ,yellow))))

   `(speedbar-button-face ((t (:foreground ,base5))))
   `(speedbar-directory-face ((t (:foreground ,blue))))
   `(speedbar-file-face ((t (:foreground ,fg))))
   `(speedbar-highlight-face ((t (:inherit highlight))))
   `(speedbar-selected-face ((t (:foreground ,magenta :underline nil :weight bold))))
   `(speedbar-tag-face ((t (:foreground ,yellow))))
   
   `(diff-hl-change ((t (:foreground ,orange :background ,orange))))
   `(diff-hl-delete ((t (:foreground ,red :background ,red))))
   `(diff-hl-insert ((t (:foreground ,green :background ,green))))
   `(diff-added ((t (:inherit hl-line :foreground ,green))))
   `(diff-changed ((t (:foreground ,violet))))
   `(diff-removed ((t (:foreground ,red :background ,base3))))
   `(diff-header ((t (:foreground ,cyan))))
   `(diff-file-header ((t (:foreground ,blue))))
   `(diff-hunk-header ((t (:foreground ,violet))))
   `(diff-refine-added ((t (:inherit diff-added :inverse-video t))))
   `(diff-refine-changed ((t (:inherit diff-changed :inverse-video t))))
   `(diff-refine-removed ((t (:inherit diff-removed :inverse-video t))))

   `(eshell-prompt ((t (:foreground ,blue :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,magenta))))
   `(eshell-ls-backup ((t (:foreground ,yellow))))
   `(eshell-ls-clutter ((t (:foreground ,red))))
   `(eshell-ls-directory ((t (:foreground ,blue))))
   `(eshell-ls-executable ((t (:foreground ,green))))
   `(eshell-ls-missing ((t (:foreground ,red))))
   `(eshell-ls-product ((t (:foreground ,orange))))
   `(eshell-ls-readonly ((t (:foreground ,orange))))
   `(eshell-ls-special ((t (:foreground ,violet))))
   `(eshell-ls-symlink ((t (:foreground ,cyan))))
   `(eshell-ls-unreadable ((t (:foreground ,base5))))
   `(ansi-color-black ((t (:foreground ,bg :background ,bg))))
   `(ansi-color-red ((t (:foreground ,red :background ,red))))
   `(ansi-color-green ((t (:foreground ,green :background ,green))))
   `(ansi-color-yellow ((t (:foreground ,yellow :background ,yellow))))
   `(ansi-color-blue ((t (:foreground ,blue :background ,blue))))
   `(ansi-color-magenta ((t (:foreground ,magenta :background ,magenta))))
   `(ansi-color-cyan ((t (:foreground ,cyan :background ,cyan))))
   `(ansi-color-white ((t (:foreground ,fg :background ,fg))))
   `(ansi-color-bright-black ((t (:foreground ,base0 :background ,base2))))
   `(ansi-color-bright-red ((t (:foreground "#ff8281" :background "#ff8281"))))
   `(ansi-color-bright-green ((t (:foreground "#a7c77c" :background "#a7c77c"))))
   `(ansi-color-bright-yellow ((t (:foreground "#eec78e" :background "#eec78e"))))
   `(ansi-color-bright-blue ((t (:foreground "#6bbbf1" :background "#6bbbf1"))))
   `(ansi-color-bright-magenta ((t (:foreground "#ce8ce2" :background "#ce8ce2"))))
   `(ansi-color-bright-cyan ((t (:foreground "#61deff" :background "#61deff"))))
   `(ansi-color-bright-white ((t (:foreground ,base8 :background ,base8))))
   
   `(compilation-column-number ((t (:inherit font-lock-comment-face))))
   `(compilation-line-number ((t (:foreground ,blue))))
   `(compilation-error ((t (:inherit error :weight bold))))
   `(compilation-warning ((t (:inherit warning :slant italic))))
   `(compilation-info ((t (:inherit success))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info))))
   `(compilation-mode-line-fail ((t (:inherit compilation-error))))

   `(TeX-error-description-error ((t (:inherit error :weight bold))))
   `(TeX-error-description-warning ((t (:inherit warning :weight bold))))
   `(TeX-error-description-tex-said ((t (:inherit success :weight bold))))

   `(outline-1 ((t (:foreground ,blue :weight bold :extend t :height 1.05))))
   `(outline-2 ((t (:foreground ,magenta :weight bold :extend t :height 1.02))))
   `(outline-3 ((t (:foreground ,violet :weight bold :extend t))))
   `(outline-4 ((t (:foreground ,pink :weight bold :extend t))))
   `(outline-5 ((t (:foreground ,yellow :weight bold :extend t))))
   `(outline-6 ((t (:foreground ,green :weight bold :extend t))))
   `(outline-7 ((t (:foreground ,orange :weight bold :extend t))))
   `(outline-8 ((t (:foreground ,cyan :weight bold :extend t))))

   `(org-archived ((t (:inherit shadow))))
   `(org-block ((t (:background ,base3 :extend t :box (:line-width (10 . -1) :color ,base3 :style flat)))))
   `(org-block-background ((t (:background ,base3 :extend t))))
   `(org-block-begin-line ((t (:inherit org-block :foreground ,base6 :box (:line-width (10 . 4) :color ,base3 :style flat)))))
   `(org-block-end-line ((t (:inherit org-block-begin-line))))
   `(org-code ((t (:inherit org-block :foreground ,orange))))
   `(org-checkbox ((t (:inherit org-todo))))
   `(org-checkbox-statistics-done ((t (:inherit org-done))))
   `(org-checkbox-statistics-todo ((t (:inherit org-todo))))
   `(org-cite ((t (:foreground ,teal))))
   `(org-cite-key ((t (:foreground ,teal))))
   `(org-date ((t (:foreground ,yellow))))
   `(org-document-info ((t (:foreground ,magenta))))
   `(org-document-title ((t (:foreground ,yellow :weight bold))))
   `(org-todo ((t (:foreground ,yellow :bold inherit))))
   `(org-headline-done ((t (:foreground ,base5))))
   `(org-done ((t (:inherit org-headline-done :strike-through nil :weight bold))))
   `(org-priority ((t (:foreground ,red))))
   `(org-drawer ((t (:inherit shadow))))
   `(org-ellipsis ((t (:foreground ,base6 :underline nil))))
   `(org-table ((t (:foreground ,pink :background ,base3 :box (:line-width (10 . -1) :color ,base3 :style flat)))))
   `(org-formula ((t (:foreground ,cyan))))   
   `(org-footnote ((t (:foreground ,orange))))
   `(org-hide ((t (:foreground ,bg))))
   `(org-latex-and-related ((t (:foreground ,base8 :weight bold))))
   `(org-link ((t (:inherit link :foreground ,blue))))
   `(org-list-dt ((t (:foreground ,yellow))))
   `(org-meta-line ((t (:inherit shadow))))
   `(org-property-value ((t (:inherit shadow))))
   `(org-special-keyword ((t (:inherit shadow))))
   `(org-tag ((t (:inherit shadow))))
   `(org-verbatim ((t (:foreground ,green))))
   `(org-warning ((t (:foreground ,yellow))))
   `(org-quote ((t (:inherit org-block :slant italic))))

   `(message-cited-text ((t (:foreground ,violet))))
   `(message-cited-text-1 ((t (:foreground ,yellow))))
   `(message-cited-text-2 ((t (:foreground ,magenta))))
   `(message-cited-text-3 ((t (:foreground ,green))))
   `(message-cited-text-4 ((t (:foreground ,green))))
   `(message-header-name ((t (:foreground ,magenta))))
   `(message-header-to ((t (:foreground ,green))))
   `(message-header-cc ((t (:foreground ,green))))
   `(message-header-subject ((t (:foreground ,yellow))))
   `(message-header-other ((t (:foreground ,blue))))
   `(message-separator ((t (:foreground ,base5))))
   `(message-signature-separator ((t (:foreground ,base5))))))

(provide-theme 'watson)
