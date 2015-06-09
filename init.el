(add-to-list 'load-path "~/.emacs.d/visuals")
(add-to-list 'load-path "~/.emacs.d/behavior")
(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/.emacs.d/custom/modes")

(defvar go-tools-binary-path
  (expand-file-name "~/go/go-tools-ws")
  "Location of go tool binaries (e.g. oracle, cover, impl, etc.)")

(setenv "PATH"
		(concat
		 go-tools-binary-path ":"
		 (getenv "PATH")))

(add-hook 'after-init-hook
	  #'(lambda ()
		  (load "functions") ; must be first
		  (load "packages")
	      (load "visuals")
	      (load "behavior")
		  (load "mode-setup")
		  (load "keybindings")
		  (load "go")
		  (load "go-helper-mode")
		  (load "ginkgo-mode")
		  (load "go-autocomplete")))

;; Added by emacs itself
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(garslo-go-tool-workspace "/usr")
 '(go-rename-command "~/bin/gorename")
 '(helm-ff-newfile-prompt-p nil)
 '(jira-url "https://jira.atlas.llnw.com/rpc/xmlrpc")
 '(python-shell-virtualenv-path "/home/garslo/storage/dev/test"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
