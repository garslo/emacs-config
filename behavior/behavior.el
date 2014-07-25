;; Don't want to type yes & no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Better searching & things
;;;; #REQUIRES icicles
(icicle-mode 1)

;; Buffers with the same names have better distinguishers
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Tidy things up before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Get rid of backup files in individual directories
(add-to-list 'backup-directory-alist '(".*" . "~/.backup"))

;; Make sure we start in the home directory
(setq default-directory "~/")
