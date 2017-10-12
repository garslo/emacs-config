(fset 'switch-to-previous-buffer-in-other-window
	  (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("obo" 0 "%d")) arg)))
