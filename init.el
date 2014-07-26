(add-to-list 'load-path "~/.emacs.d/visuals")
(add-to-list 'load-path "~/.emacs.d/behavior")
(add-to-list 'load-path "~/.emacs.d/custom")

(add-hook 'after-init-hook
	  #'(lambda ()
		  (load "packages")
		  (load "functions")
	      (load "visuals")
	      (load "behavior")
		  (load "mode-setup")
		  (load "keybindings")
		  (load "go")))

;; Added by emacs itself
(put 'dired-find-alternate-file 'disabled nil)
