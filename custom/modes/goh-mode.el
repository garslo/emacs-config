(defgroup goh nil
  "Helper functions for Go interaction"
  :group 'go)

;;
;; 1. Workspace navigation
;; 2. Repo navigation within workspaces
;;    a) Fuzzy matching on repo names
;; 3. Install package
;; 4. Set gocode library path
;; 5. Set workspace
;;    a) Set GOPATH variable

(defcustom goh-ws-base-dir-list '("~/dev/llnw")
  "Location of your -ws directories"
  :type 'string
  :group 'goh
  :safe 'string)

(defcustom goh-gen-mocks-binary "go-gen-mocks.sh"
  "Lcoation of go-gen-mocks.sh script"
  :type 'string
  :group 'goh
  :safe 'string)

(defun goh--get-package-index ()
  (go-packages))

(defun goh--get-goroot ()
  (getenv "GOROOT"))

(defun goh--get-gopath-env ()
  (getenv "GOPATH"))

(defun goh--set-gopath-env (path)
  (setenv "GOPATH" path)
  path)

(defun goh-prompt-for-gopath (path)
  (interactive "DGOPATH: ")
  (goh--set-gopath path)
  path)

(defun goh--get-gopath ()
  (let ((gopath (goh--get-gopath-env)))
	(if (not gopath)
		(goh-prompt-for-gopath)
	  gopath)))

(defun goh--ls-dir (dir)
  (split-string (shell-command-to-string (format "ls -d %s/*/" dir)) "\n"))

(defun goh--ls-dirs (dirs)
  (-flatten (mapcar 'goh--ls-dir dirs)))

(defun goh--fuzzy-find-ws ()
  (helm :sources (helm-build-async-source "workspaces"
				   :candidates-process (lambda ()
						 (goh--ls-dirs goh-ws-base-dir-list)))))

(defun goh--goto-ws (ws)
  (find-file ws))

(defun goh-set-gocode-lib-path ()
  (interactive)
  (let ((lib-path (format "%spkg/linux_amd64" (goh--get-gopath))))
	(shell-command (format "gocode set lib-path \"%s\"" lib-path))))

(defun goh-get-current-package ()
  (interactive)
  (shell-command-to-string "go list ."))

(defun goh--get-gen-mocks-cmd ()
  (let ((package (goh--fuzzy-find-package)))
	(format "%s %s" goh-gen-mocks-binary package)))

(defun goh-gen-mocks ()
  (interactive)
  (async-shell-command (goh--get-gen-mocks-cmd) "*go-gen-mocks*"))

(defun goh-switch-ws ()
  (interactive)
  (goh--set-ws (goh--fuzzy-find-ws)))

(defun goh--set-ws (ws)
  (goh--set-gopath-env ws)
  (goh-set-gocode-lib-path)
  (goh--goto-ws ws)
  (message (format "workspace is %s" ws)))

(defun goh-set-pwd-as-ws ()
  (interactive)
  (goh--set-ws (expand-file-name default-directory)))

(defun goh--fuzzy-find-package ()
  (helm :sources (helm-build-async-source "packages"
				   :candidates-process 'goh--get-package-index)))

(defun goh-switch-repo ()
  (interactive)
  (let* ((package (goh--fuzzy-find-package))
		(gopath-pkg-path (concat (goh--get-gopath) "/src/" package))
		(goroot-pkg-path (concat (goh--get-goroot) "/src/" package)))
	(cond
	 ((file-exists-p gopath-pkg-path) (find-file gopath-pkg-path))
	 ((file-exists-p goroot-pkg-path) (find-file goroot-pkg-path))
	 (t (message (format "package %s not found in GOPATH or GOROOT"))))))

(defun goh-make-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r") 'goh-switch-repo)
    (define-key map (kbd "C-c w") 'goh-switch-ws)
	(define-key map (kbd "C-c sw") 'goh-set-pwd-as-ws)
    map))

(define-minor-mode goh-mode
  "go-helper-mode (NG!)"
  :ligher " goh"
  :keymap (goh-make-keymap))

(define-globalized-minor-mode goh-global-mode
  goh-mode
  goh-on)

(defun goh-on ()
  (goh-mode 1))

(provide 'goh-mode)
