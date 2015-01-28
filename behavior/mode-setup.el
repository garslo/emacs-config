;;;; Global modes

;; auto-complete
;;;; #REQUIRES: auto-complete
(require 'auto-complete)
(global-auto-complete-mode)

;; eshell
(require 'eshell)
(require 'em-smart)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(add-hook 'eshell-mode-hook
		  (lambda ()
			(eshell-smart-initialize)))


;; gutter
;;
;; Adds git hints to "gutter"
;;
;;;; #REQUIRES: git-gutter, git-gutter-fringe
(when (window-system)
  (require 'git-gutter-fringe))

;; ido
;;
;; Better buffer switching
;;
(ido-mode 'buffers)

;; electric
;;
;; electric-pair-mode auto-inserts a right-matching
;; character when typing things like ", (, etc.
;;
(add-hook 'prog-mode-hook #'electric-pair-mode)
;; Auto-indents when pressing enter
(add-hook 'prog-mode-hook (lambda ()
                            (local-set-key (kbd "RET")
                                           'electrify-return-if-match)))

;; git-gutter
;;
;; Adds gutter notations for what's changed since last commit
;;
;(global-git-gutter-mode 1)

;; helm
;;
;; Better switching, searching
;;
(helm-mode 1)

;; jedi
;;
;; Autocomplete in python
;;
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; linum
;;
;; View line numbers when in a programmatic mode
;;
(add-to-list 'prog-mode-hook #'linum-mode)

;; markdown
(add-hook 'markdown-mode-hook
		  (lambda ()
			(linum-mode 1)  			; Add line numbers
			(orgtbl-mode 1)))			; Add beautiful ascii table support

;; multiple-cursors
;;
(require 'multiple-cursors)

;; paredit
;;
;; Wonderful lispy settings
;;
;;;; #REQUIRES: paredit
(defun my-lispy-settings ()
  (paredit-mode 1)
  (eldoc-mode 1))
(add-hook 'emacs-lisp-mode-hook 'my-lispy-settings)
(add-hook 'inferior-emacs-lisp-mode-hook 'my-lispy-settings)

;; projectile
;;
;; A sort-of generic package managing, erm, package
;;
;;;; #REQUIRES: projectile
(projectile-global-mode 1)
(setq projectile-enable-caching t)

;; subword
;;
;; Allows traversing of CamelCase as two words
;;
(global-subword-mode)
