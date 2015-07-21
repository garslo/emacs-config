;; markdown
(add-hook 'markdown-mode-hook
		  (lambda ()
			(linum-mode 1)  			; Add line numbers
			(orgtbl-mode 1)))			; Add beautiful ascii table support
