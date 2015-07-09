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

;; A better find files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; swoop!
(global-set-key (kbd "C-c C-c h") 'helm-multi-swoop-all)

;; Easier frame shortcuts
(global-set-key (kbd "C-c f") 'make-frame-command)
(global-set-key (kbd "C-c k") 'delete-frame)
(global-set-key (kbd "M-`") 'other-frame)

;; multiple-cursors
(global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-_") 'mc/mark-all-like-this)

;; avy
(global-set-key (kbd "C-c ;") 'avy-goto-word-1)
(global-set-key (kbd "C-c '") 'avy-goto-char)
