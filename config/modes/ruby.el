(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c C-p") 'ruby-send-buffer)))
(add-hook 'robe-mode-hook 'ac-robe-setup)
