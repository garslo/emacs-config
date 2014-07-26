(add-to-list 'load-path "~/.emacs.d/visuals")
(add-to-list 'load-path "~/.emacs.d/behavior")
(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/.emacs.d/custom/modes")

(defvar go-tools-binary-path
  "/Users/gslopsema/llnw/go/bin"
  "Location of go tool binaries (e.g. oracle, cover, impl, etc.)")

(setenv "PATH"
		(concat
		 go-tools-binary-path ":"
		 (getenv "PATH")))

(add-hook 'after-init-hook
	  #'(lambda ()
		  (load "packages")
		  (load "functions")
	      (load "visuals")
	      (load "behavior")
		  (load "mode-setup")
		  (load "keybindings")
		  (load "go")
		  (load "go-helper-mode")
		  (load "ginkgo-mode")))

;; Added by emacs itself
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)
