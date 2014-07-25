;;;; Global modes

;; auto-complete
;;;; #REQUIRES: auto-complete
(require 'auto-complete)
(global-auto-complete-mode)

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

;; linum
;;
;; View line numbers when in a programmatic mode
;;
(add-to-list 'prog-mode-hook #'linum-mode)

;; paredit
;;
;; Wonderful lispy settings
;;
;;;; #REQUIRES: paredit
(defun my-lispy-settings ()
  (paredit-mode))
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
