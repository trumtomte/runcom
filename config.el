;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Sebastian Bengtegård"
      user-mail-address "sebastianbengtegard@protonmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "IBM Plex Mono" :size 17 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 17))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

(setq evil-escape-key-sequence "jj")

(after! mu4e
  ;; General settings
  (setq mu4e-attachment-dir "~/Downloads"
        mu4e-headers-time-format "%R"
        mu4e-headers-date-format "%d/%m/%y"
        mm-text-html-renderer 'gnus-w3m)
  ;; Shortcuts
  (add-to-list 'mu4e-maildir-shortcuts '(:maildir "/pm/INBOX" :key ?p))
  (add-to-list 'mu4e-maildir-shortcuts '(:maildir "/doris/INBOX" :key ?d))
  ;; Accounts
  (set-email-account! "doris"
                      '((mu4e-sent-folder . "/doris/Sent")
                        (mu4e-drafts-folder . "/doris/Drafts")
                        (mu4e-trash-folder . "/doris/Trash")
                        (mu4e-refile-folder . "/doris/Archive")
                        (user-mail-address . "sebastian@doris.tech")
                        (smtpmail-smtp-service . 465)
                        (smtpmail-stream-type . ssl)
                        (smtpmail-smtp-server . "csrv11.aname.net")
                        (smtpmail-smtp-user . "sebastian@doris.tech"))
                      t)
  (set-email-account! "protonmail"
                      '((mu4e-sent-folder . "/pm/Sent")
                        (mu4e-drafts-folder . "/pm/Drafts")
                        (mu4e-trash-folder . "/pm/Trash")
                        (mu4e-refile-folder . "/pm/Archive")
                        (user-mail-address . "sebastianbengtegard@protonmail.com")
                        (smtpmail-smtp-service . 1025)
                        (smtpmail-smtp-server . "127.0.0.1")
                        (smtpmail-stream-type . plain)
                        (smtpmail-smtp-user . "sebastianbengtegard@protonmail.com"))
                      t))

;; (use-package! org-ref
;;   :after org
;;   :custom
;;   (setq bibtex-completion-bibliography '("/home/sebbe/Zotero/library.bib"))
;;   (define-key org-mode-map (kbd "C-c i") 'org-ref-insert-link)
;;   (define-key org-mode-map (kbd "C-c r") 'org-ref-insert-ref-link))
;;

(after! org
  (setq org-cite-global-bibliography '("/home/sebbe/Zotero/library.bib"))
  (setq org-latex-compiler "pdfxelatex")
  (setq org-latex-pdf-process '("latexmk -pdfxe -interaction=\"nonstopmode\" -bibtex -f %f")))

(use-package! oc-bibtex
  :after org)

(after! ox-latex
  (add-to-list 'org-latex-classes
               '("IEEEtran" "\\documentclass[10pt]{IEEEtran}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("llncs" "\\documentclass[10pt]{llncs}"
                 ("\\section{%s}" . "\\section{%s}")
                 ("\\subsection{%s}" . "\\subsection{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                 ("\\paragraph{%s}" . "\\paragraph{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph{%s}")))
  (add-to-list 'org-latex-classes
               '("apa7" "\\documentclass[10pt]{apa7}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(after! lsp-mode
  (setq lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          (shadow . t)
                          (unusedparams . t)
                          (unusedwrite . t)
                          (useany . t)
                          (unusedvariable . t))))

;; C/C++ debugging
(setq gdb-many-windows t)
(setq gdb-speedbar-auto-raise t)
(map! :localleader
      (:map (c++-mode-map c-mode-map)
            (:desc "Run GDB" "g" #'gdb)
            (:prefix ("d" . "debug")
                     (:desc "Display GDB buffer" "g" #'gdb-display-gdb-buffer)
                     (:desc "Display memory buffer" "m" #'gdb-display-memory-buffer)
                     (:desc "Display locals buffer" "l" #'gdb-display-locals-buffer)
                     (:desc "Display locals buffer" "l" #'gdb-display-locals-buffer)
                     (:desc "Display locals buffer" "l" #'gdb-display-locals-buffer)
                     (:desc "Display I/O buffer" "i" #'gdb-display-io-buffer)
                     (:desc "Run the program" "r" #'gud-run)
                     (:desc "Step one line (next)" "n" #'gud-next)
                     (:desc "Watch expression at point" "w" #'gud-watch)
                     (:desc "Set breakpoint" "b" #'gud-break)
                     (:desc "Set breakpoint" "B" #'gud-remove))))
