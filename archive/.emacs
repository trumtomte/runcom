;; --------------
;; Packages
;; --------------
(require 'package)
(setq package-archives
      '(
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

(require 'cc-mode)

;; ------------------
;; TODO/TEST
;; ------------------
;; 1. Undo-tree
;; 2. helm-swoop

;; -------------
;; Themes
;; -------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'sherlock t)
;; (load-theme 'gotham t)

(load "~/.emacs.d/highlight-sexp.el")
(load "~/.emacs.d/focus-mode.el")

;; Scratchpad @ startup
(setq inhibit-startup-screen t)

;; y/n questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Remove menu/toolbars @ window mode
(when (window-system)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1))

;; Remove blinking cursor
(blink-cursor-mode -1)

;; Remove sound on errors
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Increase/Decrease font size
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-+") 'text-scale-increase)

;; -----------
;; Mouse & Scroll
;; -----------
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq smooth-scroll-margin 5)

;; Automatically reread files when changed
(global-auto-revert-mode t)

;; --------------
;; Encoding
;; --------------
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
;; --------------
;; Evil mode
;; --------------
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "U") 'undo-tree-redo)
;; Escape from insert mode
(define-key evil-insert-state-map (kbd "C-j") 'evil-force-normal-state)
;; Retain emacs key chords for insert/normal states
(define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
(define-key evil-visual-state-map (kbd "C-a") 'evil-beginning-of-line)
(define-key evil-normal-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-normal-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-normal-state-map (kbd "C-r") 'isearch-backward)
;; (define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)

;; Offset scrolling
(define-key evil-normal-state-map (kbd "C-k") (lambda ()
(interactive)
(evil-scroll-line-up 3)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
(interactive)
(evil-scroll-line-down 3)))
;; Switch between windows
(defun back-window ()
"Backwards switching of windows."
(interactive)
(other-window -1))
(define-key evil-normal-state-map (kbd "<tab>") 'other-window)
(define-key evil-normal-state-map (kbd "<backtab>") 'back-window)

;; -----------
;; ERC
;; -----------

(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
'((".*\\.freenode.net"
;; "#emacs"
;; "#vim"
"##broders" ; personal channel
"#haskell"
"##javascript")))

(defun erc-connect-or-switch ()
"Connect to ERC or switch to current active buffer"
(interactive)
(if (get-buffer "irc.freenode.net:6667")
(erc-track-switch-buffer 1)
(when (y-or-n-p "Start ERC? ")
(erc :server "irc.freenode.net" :port 6667 :nick "sebbarn" :full-name "Sebastian"))))

(global-set-key [f6] 'erc-connect-or-switch)

(erc-track-mode t)
(setq erc-track-exclude-types
'("JOIN" "NICK" "PART" "QUIT" "MODE"
"324" "329" "332" "333" "353" "477"))

;; Shorter forms
(erc-define-catalog-entry 'english 'JOIN
"%n has joined channel %c")
(erc-define-catalog-entry 'english 'QUIT
"%n has quit: %r")

(setq erc-hide-list
'("PART" "NICK" "JOIN" "QUIT"))

;; (erc-scrolltobottom-enable)
(setq erc-max-buffer-size 20000)

;; Fill
;; (setq erc-fill-function 'erc-fill-static)
;; (setq erc-fill-static-center 15)

(require 'erc-stamp)
(erc-stamp-mode 1)
(setq erc-timestamp-only-if-changed-flag nil
erc-insert-timestamp-function 'erc-insert-timestamp-left
erc-insert-away-timestamp-function 'erc-insert-timestamp-left
erc-timestamp-format "[%H:%M]"
erc-away-timestamp-format "<%H:%M>"
erc-fill-prefix "       ")

;; -------------------
;; Custom splitting
;; -------------------
(defun vsplit-last-buffer ()
(interactive)
(split-window-vertically)
(other-window 1 nil)
(switch-to-next-buffer))
(defun hsplit-last-buffer ()
(interactive)
(split-window-horizontally)
(other-window 1 nil)
(switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

;; ------------
;; Modifiers
;; ------------
(setq ns-function-modifier 'control)
(setq ns-command-modifier 'meta)
(setq ns-option-modifier nil)

;; ---------
;; Backup
;; ---------
(setq backup-by-copying t
backup-directory-alist '(("." . "~/.emacs.d/saves"))
delete-old-versions t
kept-new-versions 6
kept-old-versions 2
version-control t)

;; ----------
;; Git
;; ----------
(require 'magit)
(require 'git-gutter-fringe)

;; TODO: better keybinds for these
;; (global-set-key (kbd "C-c m") 'magit-merge)
;; (global-set-key (kbd "C-c c") 'magit-checkout)
;; (global-set-key (kbd "C-c p") 'magit-push)
(global-set-key (kbd "C-c g") 'magit-status)
(global-git-gutter-mode +1)

;; ------------
;; Bindings
;; ------------
(global-set-key (kbd "M-§") 'other-window)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; --------------
;; Doc View mode
;; --------------
(defun switch-window-doc-next ()
"Switch to other window and go to next page."
(interactive)
(other-window 1)
(doc-view-next-page)
(other-window 1))

(defun switch-window-doc-prev ()
"Switch to other window and go to previous page."
(interactive)
(other-window 1)
(doc-view-previous-page)
(other-window 1))

;; Navigate to next or previous page without leavning the current window
(global-set-key (kbd "C-c C-n") 'switch-window-doc-next)
(global-set-key (kbd "C-c C-p") 'switch-window-doc-prev)

;; --------------
;; Eshell
;; --------------
(defun create-new-eshell ()
"Create a new eshell with a given name."
(interactive)
(let ((buf (get-buffer-create
(concat "*" (read-string "Eshell name: " nil) "*"))))
(pop-to-buffer-same-window buf)
(unless (derived-mode-p 'eshell-mode)
(eshell-mode))))

(global-set-key [f1] 'eshell)
(global-set-key [f2] 'create-new-eshell)

;; $PATH for eshell
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "LANG" "en_GB.UTF-8")
(setenv "LC_ALL" "en_GB.UTF-8")
(setenv "LC_CTYPE" "en_GB.UTF-8")

;; Settings
(setq eshell-history-size 20000)
(setq eshell-save-history-on-exit t)
(setq eshell-hist-ignoredups t)

;; eshell prompt
(setq eshell-prompt-function (lambda ()
(concat
(propertize (format-time-string "%H:%M " (current-time)) 'face `(:foreground "#777777"))
(propertize "• " 'face `(:foreground "#cc2f47"))
(propertize "➜ " 'face `(:foreground "#c1ae6e")))))
(setq eshell-highlight-prompt nil)

;; Ansi color - not working?
(ansi-color-for-comint-mode-on)
(defun eshell-handle-ansi-color ()
(ansi-color-apply-on-region eshell-last-output-start
eshell-last-output-end))
(add-hook 'eshell-mode-hook
'(lambda ()
(add-to-list
'eshell-output-filter-functions
'eshell-handle-ansi-color)))


;; --------------
;; Auto modes & Hooks
;; --------------
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(setq highlight-symbol-idle-delay 0) ; Delay until symbol is highlighted

;; need to require different modes?

;; Set major modes based on file extensions
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'|\\.jshintrc\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'|\\.markdown\\'|\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))
(add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.jade" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))

;; (setq auto-mode-alist
;;       (append '(("..." . mode-name)
;; 		("..." . mode-name))
;; 	      ))

(setq scss-compile-at-save nil)

(defun paredit-nolisp ()
"Turn on paredit for non-lisps."
(interactive)
(set (make-local-variable 'paredit-space-for-delimiter-predicates)
'((lambda (endp delimiter) nil)))
(paredit-mode 1))

(add-hook 'js2-mode-hook 'paredit-nolisp)
(add-hook 'php-mode 'paredit-nolisp)
(add-hook 'web-mode 'paredit-nolisp)
(add-hook 'scss-mode 'paredit-nolisp)
(add-hook 'markdown-mode 'paredit-nolisp)

;; -----------------------
;; Node.js / JavaScript
;; -----------------------
(require 'nodejs-repl)
(setq nodejs-repl-command "/usr/local/bin/node")

(defun send-region-to-nodejs-repl-process (start end)
"Send region to `nodejs-repl` process."
(interactive "r")
(save-selected-window
(save-excursion (nodejs-repl)))
(comint-send-region (get-process nodejs-repl-process-name) start end))

(add-hook 'js2-mode-hook
(lambda ()
(define-key evil-visual-state-local-map (kbd "C-x C-e") 'send-region-to-nodejs-repl-process)
(define-key js2-mode-map "{" 'paredit-open-curly)
(define-key js2-mode-map "}" 'paredit-close-curly)))

;; Use spaces as default
(setq-default indent-tabs-mode nil)

;; ------------------
;; Neotree
;; ------------------
(require 'neotree)
(global-set-key (kbd "C-x C-t") 'neotree-toggle)
(global-set-key [f5] 'neotree-toggle)

;; Evil mode with neotree
(add-hook 'neotree-mode-hook
(lambda ()
(define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
(define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; ---------------------
;; Helm / Projectile
;; ---------------------
(require 'helm)
(require 'helm-config)

(when (executable-find "/usr/bin/curl")
(setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t
helm-buffers-fuzzy-matching t
helm-move-to-line-cycle-in-source t
helm-ff-search-library-in-sexp t
helm-ff-file-name-history-use-recentf t
helm-scroll-amount 8
helm-adaptive-history-file "~/.emacs.d/data/helm-history")

(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-m") 'helm-imenu)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Bound to C-c h C-c g, rebind?
;; helm-google-suggets

(helm-mode 1)

(projectile-global-mode)
(setq projectile-completion-system 'helm
      projectile-enable-caching t)
(helm-projectile-on)

(global-set-key (kbd "C-x C-p") 'helm-projectile-find-file)

;; ----------------------
;; Unique buffer names
;; ----------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t)

;; ---------------
;; iBuffer
;; ---------------
(require 'ibuffer)
(global-set-key (kbd "C-x b") 'ibuffer)

;; -----------------
;; Gnus (mail)
;; -----------------
(require 'gnus)
(setq nnml-directory "~/Maildir")
(setq message-directory "~/Maildir")

(setq gnus-select-method
      '(nnimap "outlook"
	       (nnimap-address "outlook.office365.com")
	       (nnimap-server-port 993)))

(setq-default
 gnus-summary-line-format "[%U%R%z]  %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-single-indent ""
 gnus-sum-thread-tree-single-leaf "└─>"
 gnus-sum-thread-tree-vertical "│"
 gnus-sum-thread-tree-leaf-with-other "├─>")

(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
	(not gnus-thread-sort-by-number)))

(setq gnus-use-cache t)
(setq gnus-use-adaptive-scoring t)
(setq gnus-save-score t)

;; -----------------
;; Mode line
;; -----------------
(setq-default
 mode-line-format
 '(
   "  "
   ;; Read only & modified
   (:eval
    (cond (buffer-read-only
	   (propertize "RO" 'face 'mode-line-read-only-face))
	  ((buffer-modified-p)
	   (propertize "++" 'face 'mode-line-modified-face))
	  (t "  ")))
   "  "
   ;; Evil mode
   (:eval
    (cond ((evil-insert-state-p)
	   (propertize " i " 'face 'mode-line-evil-insert-state-face))
	  ((evil-visual-state-p)
	   (propertize " v " 'face 'mode-line-evil-visual-state-face))
	  ((evil-normal-state-p)
	   (propertize " n " 'face 'mode-line-evil-normal-state-face))
	  (t "   ")))
   "  "
   ;; Short directory
   (:propertize (:eval
		 (shorten-directory default-directory 30))
		face mode-line-folder-face)
   ;; Filename
   (:propertize "%b" face mode-line-filename-face)
   "  "
   ;; Major mode
   (:propertize mode-name face mode-line-mode-face)
   "  "
   ;; Version control
   (:eval
    (cond (vc-mode
	   (propertize (concat "(" vc-mode " )") 'face 'mode-line-vc-face))
	  (t "")))
   ))

;; Shorten directory names (cant remember where i found this, ought to learn doing this myself)
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
	(output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-evil-normal-state-face)
(make-face 'mode-line-evil-insert-state-face)
(make-face 'mode-line-evil-visual-state-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-vc-face)

;; Face attributes
(set-face-attribute 'mode-line nil
		    :background "#000000"
		    :foreground "#c1ae6e"
		    :inverse-video nil
		    :box '(:line-width 4 :color "#000000" :style nil))
(set-face-attribute 'mode-line-read-only-face nil
		    :inherit 'mode-line-face
		    :foreground "#cc2f47")
(set-face-attribute 'mode-line-modified-face nil
		    :inherit 'mode-line-face
		    :foreground "#cc2f47")
(set-face-attribute 'mode-line-folder-face nil
		    :inherit 'mode-line-face
		    :foreground "#777777")
(set-face-attribute 'mode-line-mode-face nil
		    :inherit 'mode-line-face
		    :foreground "#7c96bf")
(set-face-attribute 'mode-line-evil-normal-state-face nil
		    :inherit 'mode-line-face
		    :foreground "#888888")
(set-face-attribute 'mode-line-evil-insert-state-face nil
		    :inherit 'mode-line-face
		    :foreground "#888888")
(set-face-attribute 'mode-line-evil-visual-state-face nil
		    :inherit 'mode-line-face
		    :foreground "#888888")
(set-face-attribute 'mode-line-vc-face nil
		    :inherit 'mode-line-face
		    :foreground "#777777")
(set-face-attribute 'mode-line-inactive nil
		    :background "#1a1a1a"
		    :foreground "#777777"
		    :inverse-video nil
		    :box '(:line-width 4 :color "#1a1a1a" :style nil))

;; Unused
(set-face-attribute 'mode-line-buffer-id nil)
(set-face-attribute 'mode-line-emphasis nil)
(set-face-attribute 'mode-line-highlight nil)

;; -----------------
;; Appearance
;; TODO: these should be placed in the color theme?
;; -----------------

;; Cursor
(set-cursor-color "#c1ae6e")

;; Font face
(set-face-attribute 'default nil
		    :family "Menlo"
		    :height 130
		    :weight 'normal
		    :width 'normal)

;; Line wrap arrows
(set-face-attribute 'fringe nil
		    :foreground "#777777")

;; Custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-proprietary-property ((t (:inherit css-property))))
 '(css-selector ((t (:foreground "#c1ae6e"))))
 '(erc-button ((t (:underline t))))
 '(erc-current-nick-face ((t (:foreground "#c1ae6e"))))
 '(erc-default-face ((t (:foreground "#cfcaaa"))))
 '(erc-direct-msg-face ((t (:foreground "#fbf8ff"))))
 '(erc-error-face ((t (:foreground "#cc2f47"))))
 '(erc-input-face ((t (:foreground "#c1ae6e"))))
 '(erc-keyword-face ((t (:foreground "#c1ae6e"))))
 '(erc-my-nick-face ((t (:foreground "#c1ae6e"))))
 '(erc-nick-default-face ((t (:foreground "#7c96bf"))))
 '(erc-nick-msg-face ((t (:foreground "#cc2f47"))))
 '(erc-notice-face ((t (:foreground "#777777"))))
 '(erc-prompt-face ((t (:foreground "#c1ae6e"))))
 '(erc-timestamp-face ((t (:foreground "#777777"))))
 '(eshell-ls-archive ((t (:foreground "#c1ae6e"))))
 '(eshell-ls-directory ((t (:foreground "#7c96bf"))))
 '(eshell-ls-executable ((t (:foreground "#cc2f47"))))
 '(eshell-ls-readonly ((t (:foreground "#cc2f47"))))
 '(eshell-ls-symlink ((t (:foreground "#fbf8ff"))))
 '(eshell-prompt ((t :foreground "#c1ae6e")))
 '(font-lock-builtin-face ((t (:foreground "#7c96bf"))))
 '(gnus-summary-normal-ancient ((t (:foreground "#cfcaaa"))))
 '(helm-M-x-key ((t (:foreground "#cc2f47"))))
 '(helm-buffer-directory ((t :foreground "#c1ae6e")))
 '(helm-buffer-process ((t (:foreground "#c1ae6e"))))
 '(helm-buffer-size ((t (:foreground "#fbf8ff"))))
 '(helm-candidate-number ((t :background "#000000" :foreground "#cc2f45")))
 '(helm-ff-directory ((t :foreground "#c1ae6e")))
 '(helm-ff-executable ((t :foreground "#fbf8ff")))
 '(helm-ff-file ((t :foreground "#7c96bf")))
 '(helm-ff-symlink ((t (:foreground "#cc2f47"))))
 '(helm-history-remote ((t (:foreground "#cc2f47"))))
 '(helm-selection ((t :background "#444444")))
 '(helm-source-header ((t :background "#222222" :foreground "#c1ae6e")))
 '(highlight ((t (:background "yellow" :foreground "black"))))
 '(highlight-numbers-number ((t (:foreground "#7c96bf"))))
 '(highlight-quoted-symbol ((t (:foreground "#c1ae6e"))))
 '(highlight-symbol-face ((t (:background "#444444"))))
 '(isearch-fail ((t (:background "Red" :foreground "#cfcaaa"))))
 '(js2-external-variable ((t (:foreground "#fbf8ff"))))
 '(js2-function-call ((t (:foreground "#cfcaaa"))))
 '(js2-function-param ((t (:foreground "#cfcaaa"))))
 '(js2-warning ((t (:underline "red"))))
 '(lazy-highlight ((t (:background "#c1ae6e" :foreground "black"))))
 '(markdown-bold-face ((t (:foreground "#fbf8ff"))))
 '(markdown-header-delimiter-face ((t (:foreground "#c1ae6e"))))
 '(markdown-header-face ((t (:foreground "#fbf8ff"))))
 '(markdown-header-rule-face ((t (:inherit font-lock-function-name-face))))
 '(markdown-inline-code-face ((t (:foreground "#cc2f47"))))
 '(markdown-italic-face ((t (:foreground "#fbf8ff"))))
 '(markdown-list-face ((t (:foreground "#7c96bf"))))
 '(markdown-pre-face ((t (:foreground "#7c96bf"))))
 '(minibuffer-prompt ((t :foreground "#c1ae6e")))
 '(neo-dir-link-face ((t (:foreground "#c1ae6e"))))
 '(neo-expand-btn-face ((t (:foreground "#fbf8ff"))))
 '(neo-file-link-face ((t (:foreground "#cfcaaa"))))
 '(neo-header-face ((t (:foreground "#7c96bf"))))
 '(rainbow-delimiters-depth-1-face ((t :foreground "#fbf8ff")))
 '(rainbow-delimiters-depth-2-face ((t :foreground "#fbf8ff")))
 '(rainbow-delimiters-depth-3-face ((t :foreground "#fbf8ff")))
 '(rainbow-delimiters-depth-4-face ((t :foreground "#fbf8ff")))
 '(rainbow-delimiters-depth-5-face ((t :foreground "#fbf8ff")))
 '(rainbow-delimiters-depth-6-face ((t :foreground "#fbf8ff")))
 '(rainbow-delimiters-depth-7-face ((t :foreground "#fbf8ff")))
 '(rainbow-delimiters-depth-8-face ((t :foreground "#fbf8ff")))
 '(rainbow-delimiters-depth-9-face ((t :foreground "#fbf8ff"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#cc2f47" "#7c96bf" "#c1ae6e" "#7c96bf" "#cfcaaa" "#c1ae6e" "#fbf8ff"])
 '(custom-safe-themes
   (quote
    ("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" default)))
 '(doc-view-ghostscript-program "/usr/local/bin/gs")
 '(erc-prompt ">")
 '(eshell-visual-commands
   (quote
    ("vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm")))
 '(eshell-visual-subcommands nil)
 '(focus-working-area-width 800)
 '(hl-paren-colors (quote ("#fbf8ff" "#e2dfe5" "#bcbabf" "#7d7c7f")))
 '(hl-sexp-background-color "#202020")
 '(ibuffer-marked-face (quote font-lock-string-face))
 '(js2-global-externs nil)
 '(js2-highlight-level 3)
 '(js2-include-node-externs t)
 '(js2-indent-switch-body t)
 '(magit-diff-options nil)
 '(neo-banner-message "Project tree")
 '(php-mode-coding-style (quote psr2)))

