(require 'grizzl)

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

(defcustom goh-ws-base-dir "~/dev/llnw"
  "Location of your -ws directories"
  :type 'string
  :group 'goh
  :safe 'string)

(defvar goh--package-index nil
  "Cached search index for grizzl")

(defun goh--wipe-package-index ()
  (setq goh--package-index nil))

(defun goh--make-package-index ()
  (message "Creating one-time package index...")
  (let ((packages (split-string (shell-command-to-string "go list git.llnw.com/...") "\n")))
	(setq goh--package-index (grizzl-make-index packages))))

(defun goh--get-package-index ()
  (if (not goh--package-index)
	  (goh--make-package-index)
	goh--package-index))

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

(defun goh--ls-dirs (dir)
  (split-string (shell-command-to-string (format "ls -d %s/*/" dir)) "\n"))

(defun goh--fuzzy-find-ws ()
  (let ((search-index (grizzl-make-index (goh--ls-dirs goh-ws-base-dir))))
	(grizzl-completing-read "Workspace: " search-index)))

(defun goh--goto-ws (ws)
  (find-file ws))

(defun goh-set-gocode-lib-path ()
  (interactive)
  (let ((lib-path (format "%spkg/linux_amd64" (goh--get-gopath))))
	(shell-command (format "gocode set lib-path \"%s\"" lib-path))))

(defun goh-switch-ws ()
  (interactive)
  (let ((ws (goh--fuzzy-find-ws)))
	(goh--wipe-package-index)
	(goh--set-gopath-env ws)
	(goh-set-gocode-lib-path)
	(goh--goto-ws ws)))

(defun goh--fuzzy-find-package ()
  (grizzl-completing-read "Package: " (goh--get-package-index)))

(defun goh-switch-repo ()
  (interactive)
  (let ((package (goh--fuzzy-find-package)))
	(find-file (concat (goh--get-gopath) "/src/" package))))

(defun goh-wipe-package-index ()
  (interactive)
  (goh--wipe-package-index)
  (message "package index wiped"))

(defun goh-make-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r") 'goh-switch-repo)
    (define-key map (kbd "C-c w") 'goh-switch-ws)
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
