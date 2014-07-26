;;;; Keybindings
(add-hook 'go-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c g r") 'go-run)
			(local-set-key (kbd "C-c t") 'go-test-current-project)))
