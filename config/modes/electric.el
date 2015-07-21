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
(electric-indent-mode -1)
