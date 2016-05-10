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
(add-hook 'lisp-mode-hook 'my-lispy-settings)
(add-hook 'clojure-mode-hook 'my-lispy-settings)
(add-to-list 'auto-mode-alist '("\\.gsp\\'" . lisp-mode))
