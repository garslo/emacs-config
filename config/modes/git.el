;; git
(add-hook 'git-commit-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c g v") 'git-get-version)))
