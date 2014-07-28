;; Get rid of annoying minimize binding
(global-set-key (kbd "C-x C-z") nil)

;; Pull up magit interface
;;;; #REQUIRES: magit
(global-set-key (kbd "C-c C-g") 'magit-status)

;; Useful for aligning text
(global-set-key (kbd "C-c C-a") 'align-regexp)

;; Remove all excess whitespace in a buffer.
(global-set-key (kbd "C-c C-q") 'delete-trailing-whitespace)

;; Replace all occurrences of a string in region
(global-set-key (kbd "C-c C-r") 'replace-string)

;; Maps C-a to hop between the true end of line and the indented
;; position of the text.
;;;; #REQUIRES: functions/smarter-move-beginning-of-line
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Allow use of Mac Command key as meta
(setq ns-command-modifier (quote meta))

;; A handy popup
(global-set-key (kbd "C-c h") 'helm-mini)
