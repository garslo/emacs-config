(defgroup garslo-go-customization nil
  "Customization and setup for go"
  :group 'go)

(defcustom garslo-go-tool-workspace (expand-file-name "~/go/go-tools-ws")
  "Location of workspace containing go tools (like impl, oracle, etc.)"
  :type 'string
  :group 'garslo-go-customization
  :safe 'stringp)

(defun garslo-go--path (subdir)
  (concat
   garslo-go-tool-workspace
   "/"
   subdir))

;; gocode
(add-to-list 'load-path (garslo-go--path "src/github.com/nsf/gocode/emacs"))
(require 'go-autocomplete)
(require 'auto-complete-config)

;; goimports is a better gofmt
(setq gofmt-command (garslo-go--path "bin/goimports"))
(add-hook 'before-save-hook 'gofmt-before-save)

;; oracle
;(add-to-list 'load-path (garslo-go--path "src/code.google.com/p/go.tools/cmd/oracle"))
;(require 'go-oracle)
(defun go-oracle-mode (x) nil)

;; go-helper
(require 'go-helper-mode)
(go-helper-global-mode 1)

;; ginkgo
(require 'ginkgo-mode)

;; hook
(add-hook 'go-mode-hook (lambda ()
                          (go-eldoc-setup)
                          (ginkgo-mode)
						  ))

;;;; Keybindings
(add-hook 'go-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c g r") 'go-run)
			(local-set-key (kbd "C-c c") '(lambda ()
											(interactive)
											(shell-command "go build")))
			(local-set-key (kbd "C-c t p") 'go-test-current-project)
			(local-set-key (kbd "C-c t f") 'go-test-current-file)
			(local-set-key (kbd "C-c t t") 'go-test-current-test)
			(local-set-key (kbd "C-c g e") 'go-errcheck)
			(local-set-key (kbd "C-c l") 'golint)))
