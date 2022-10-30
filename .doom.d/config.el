;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sebastian Bengtegård"
      user-mail-address "sebastianbengtegard@protonmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "JetBrains Mono NL" :size 16 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Inter" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq-default evil-escape-key-sequence "jj")

(setq calendar-week-start-day 1)

;; Ignore android and ios folders
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\android\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\ios\\'"))

;; TODO: find out how to append, wordpress, (in time) to the type '(repeat string)
(setq lsp-intelephense-stubs
  ["apache" "bcmath" "bz2" "calendar" "com_dotnet" "Core" "ctype" "curl" "date"
   "dba" "dom" "enchant" "exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash"
   "iconv" "imap" "interbase" "intl" "json" "ldap" "libxml" "mbstring" "mcrypt"
   "meta" "mssql" "mysqli" "oci8" "odbc" "openssl" "pcntl" "pcre" "PDO"
   "pdo_ibm" "pdo_mysql" "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix"
   "pspell" "readline" "recode" "Reflection" "regex" "session" "shmop"
   "SimpleXML" "snmp" "soap" "sockets" "sodium" "SPL" "sqlite3" "standard"
   "superglobals" "sybase" "sysvmsg" "sysvsem" "sysvshm" "tidy" "tokenizer"
   "wordpress" "wddx" "xml" "xmlreader" "xmlrpc" "xmlwriter" "Zend OPcache"
   "zip" "zlib"])

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(after! mu4e
  ;; General settings
  (setq mu4e-view-show-addresses t
        mu4e-attachment-dir "~/Downloads"
        mu4e-headers-time-format "%R"
        mu4e-headers-date-format "%d/%m/%y"
        mm-text-html-renderer 'gnus-w3m)
        ;; mm-text-html-renderer 'w3m-standalone
        ;; mm-discouraged-alternatives '("text/html" "text/richtext")
        ;; mu4e-html2text-command "w3m -dump -T text/html -cols 80 -o display_link_number=true")
  ;; Bookmarks
  (add-to-list 'mu4e-bookmarks
               '(:name "Inbox - Protonmail"
                 :query "maildir:/pm/INBOX"
                 :key ?p))
  (add-to-list 'mu4e-bookmarks
               '(:name "Inbox - Doris"
                 :query "maildir:/doris/INBOX"
                 :key ?d))
  ;; Accounts
  (set-email-account! "doris"
                      '((mu4e-sent-folder . "/doris/Sent Messages")
                        (mu4e-drafts-folder . "/doris/Drafts")
                        (mu4e-trash-folder . "/doris/Deleted Messages")
                        (mu4e-refile-folder . "/doris/Archive")
                        (user-mail-address . "sebastian@doris.tech")
                        (smtpmail-smtp-service . 587)
                        (smtpmail-smtp-server . "smtp01.binero.se")
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
                        (smtpmail-stream-type . 'starttls)
                        (smtpmail-smtp-user . "sebastianbengtegard@protonmail.com"))
                      t)
  )

(provide 'config)
;;; config.el ends here
