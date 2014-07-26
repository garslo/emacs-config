;;;; Keybindings
(add-hook 'go-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c g r") 'go-run)
			(local-set-key (kbd "C-c t") 'go-test-current-project)))

;; gocode
(add-to-list 'load-path "/Users/gslopsema/llnw/go/src/github.com/nsf/gocode/emacs")
(require 'go-autocomplete)
(require 'auto-complete-config)

;; go-errcheck


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
