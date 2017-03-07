(defgroup garslo-go-customization nil
  "Customization and setup for go"
  :group 'go)

;; gocode
(require 'go-autocomplete)
(require 'auto-complete-config)

;; goimports is a better gofmt
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; go-helper
(require 'go-helper-mode)
(go-helper-global-mode 1)

;; goh
(require 'goh-mode)
(goh-global-mode 1)

;; ginkgo
(require 'ginkgo-mode)

(require 'go-guru)

;; hook
(add-hook 'go-mode-hook (lambda ()
                          (go-eldoc-setup)
						  (ginkgo-mode-on)))

;;;; Keybindings
(add-hook 'go-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c g r") 'go-run)
			(local-set-key (kbd "C-c c") '(lambda ()
											(interactive)
											(shell-command "go build")))
			(local-set-key (kbd "C-c g e") 'go-errcheck)
			(local-set-key (kbd "C-c l") 'golint)
			(local-set-key (kbd "C-c mi") 'gobb-make-interface)
			(local-set-key (kbd "C-c mb") 'gobb-make-builder)
			(local-set-key (kbd "C-c mm") 'gobb-make-builder-and-interface)))
