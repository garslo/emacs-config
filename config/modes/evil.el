;; evil
(add-hook 'evil-after-load-hook
		  (lambda ()
			(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)))
