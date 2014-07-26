(defvar *go-helper-ws-base-path* "/Users/gslopsema/vagrant/llnw/go")
(defvar *go-helper-current-ws* nil)
(defvar *go-root* "/usr/local/go")

(defun go-helper-ws-base-as-dir ()
  (file-name-as-directory *go-helper-ws-base-path*))

(defun update-gopath (gopath)
  (setenv "GOPATH" gopath))

(defun go-helper-goto-workspace (ws)
  "Opens a dired directory in the given workspace."
  (interactive "sWorkspace name (without '-ws'): ")
  (let* ((base-dir (go-helper-ws-base-as-dir))
         (ws-dir (concat base-dir ws "-ws")))
    (setq *go-helper-current-ws* (concat ws "-ws"))
    (update-gopath ws-dir)
    (find-file ws-dir)))

(defun go-helper-goto-repo (repo)
  "Opens a dired directory in the given repo. This requires
*go-helper-current-ws* to be set; call go-helper-goto-workspace
to set this variable."
  (interactive "sRepo name (without '.git'): ")
  (if (null *go-helper-current-ws*)
      (message "No workspace set. Call go-helper-goto-workspace.")
    (let* ((path-list `(,*go-helper-ws-base-path* ,*go-helper-current-ws* "src" "git.llnw.com" "lama"))
           (base-dir (concat
                      (mapconcat 'file-name-as-directory path-list "")))
           (repo-name (concat repo ".git"))
           (repo-dir (mapconcat #'file-name-as-directory `(,base-dir ,repo-name) "")))
      (find-file repo-dir)
      (go-helper-set-oracle-scope))))

(defun go-helper-set-oracle-scope ()
  (interactive)
  (let* ((gopath (getenv "GOPATH"))
        (repo-dir (expand-file-name default-directory))
        (current-package (go-helper-path-subtract gopath repo-dir)))
    (setq go-oracle-scope current-package)
    (message "Oracle scope: %s" current-package)))

(defun go-helper-set-gocode-lib-path ()
  (interactive)
  (let* ((gopath (getenv "GOPATH"))
         (command-string (format "gocode set lib-path \"%s/pkg/linux_amd64\"" gopath)))
    (shell-command command-string)))

(defun go-helper-install-subpackages ()
  (interactive)
  (let* ((command-string "go install `go list ./...`"))
    (shell-command command-string)))

(defun go-helper-path-subtract (base child)
  (if (not (go-helper-is-subdir base child))
      (message "%s is not a subdir of %s" child base)
    (let* ((start-idx (+ (length base) 5))
           (end-idx (- (length child) 1)))
      (substring child start-idx end-idx))))

(defun go-helper-is-subdir (base child)
  (let ((base-len (length base)))
    (if (>= base-len (length child))
        nil
      (equal base
         (substring child 0 base-len)))))


(defun go-helper-make-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r") 'go-helper-goto-repo)
    (define-key map (kbd "C-c w") 'go-helper-goto-workspace)
    (define-key map (kbd "C-c ss") 'go-helper-set-oracle-scope)
    (define-key map (kbd "C-c sg") 'go-helper-set-gocode-lib-path)
    (define-key map (kbd "C-c is") 'go-helper-install-subpackages)
    map))

(define-minor-mode go-helper-mode
  "Helper for go interactions"
  :lighter " GoHelper"
  :keymap (go-helper-make-keymap)
  (setenv "GOROOT" *go-root*))

(define-globalized-minor-mode go-helper-global-mode
  go-helper-mode
  go-helper-on)

(defun go-helper-on ()
  (go-helper-mode 1))

(provide 'go-helper-mode)
