;; gocode
(add-to-list 'load-path "/Users/gslopsema/llnw/go/src/github.com/nsf/gocode/emacs")
(require 'go-autocomplete)
(require 'auto-complete-config)

;; goimports is a better gofmt
(setq gofmt-command "/Users/gslopsema/llnw/go/bin/goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; oracle
(add-to-list 'load-path "/Users/gslopsema/llnw/go/src/code.google.com/p/go.tools/cmd/oracle")
(require 'go-oracle)

;; go-helper
(require 'go-helper-mode)
(go-helper-global-mode 1)

;; ginkgo
(require 'ginkgo-mode)

;; hook
(add-hook 'go-mode-hook (lambda ()
                          (go-eldoc-setup)
                          (ginkgo-mode)
                          (go-oracle-mode t)))

;;;; Keybindings
(add-hook 'go-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c g r") 'go-run)
			(local-set-key (kbd "C-c c") '(lambda ()
											(interactive)
											(command "go build")))
			(local-set-key (kbd "C-c t p") 'go-test-current-project)
			(local-set-key (kbd "C-c t f") 'go-test-current-file)
			(local-set-key (kbd "C-c t t") 'go-test-current-test)
			(local-set-key (kbd "C-c g e") 'go-errcheck)))
