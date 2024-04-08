(setq user-full-name "Sebastian Bengtegård"
      user-mail-address "sebastianbengtegard@protonmail.com")

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "127.0.0.1"
      smtpmail-smtp-service 1025)

(setq gnus-select-method '(nnnil nil))

(setq gnus-secondary-select-methods
      '((nnimap "protonmail"
                (nnimap-address "127.0.0.1")
                (nnimap-server-port 1143)
                (nnimap-stream starttls)
		(nnimap-inbox "INBOX")
		(nnimap-expiry-wait never))))

(setq gnus-use-cache t)
(setq gnus-summary-line-format "%U%R %d  %-25,25n  %B%s\n")
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
(setq mm-text-html-renderer 'gnus-w3m)

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))

(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-topic-topology '(("Gnus" visible)
				 (("protonmail" visible))))

     (setq gnus-topic-alist '(("protonmail"
                               "nnimap+protonmail:INBOX"
                               "nnimap+protonmail:Sent"
                               "nnimap+protonmail:Archive"
                               "nnimap+protonmail:Trash"
                               "nnimap+protonmail:All Mail"
			       "nnimap+protonmail:Drafts"
			       "nnimap+protonmail:Starred"
			       "nnimap+protonmail:Spam")
                              ("Gnus")))

     (gnus-topic-set-parameters "protonmail" '((display . all)))))
