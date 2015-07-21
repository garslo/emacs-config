;; jedi
;;
;; Autocomplete in python
;;
(add-hook 'python-mode-hook (lambda ()
							  (jedi:setup)
							  (local-set-key (kbd "C-c c") 'py-autopep8)
							  (local-set-key (kbd "C-c C-s") 'helm-multi-swoop-all)))
(setq jedi:complete-on-dot t)
