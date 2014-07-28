;;; go-helper-mode.el --- Helper functions for navigating LLNW's Go structure

;; Copyright (C) 2014 Gary Slopsema

;; Author: Gary Slopsema <gslopsema@gmail.com>
;; Version: 20140728.1

;; This file is NOT part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(defgroup go-helper nil
  "Helper functions for navigating LLNW's Go structure"
  :group 'go)

(defcustom go-helper-ws-base-path "/Users/gslopsema/vagrant/llnw/go"
  "Base workspace path (directory path which contains all your *-ws dirs)"
  :type 'string
  :group 'go-helper
  :safe 'string)

(defcustom go-root "/usr/local/go"
  "GOROOT env variable"
  :type 'string
  :group 'go-helper
  :safe 'string)

(defvar *go-helper-current-ws* nil)

(defun go-helper-ws-base-as-dir ()
  (file-name-as-directory go-helper-ws-base-path))

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
    (let* ((path-list `(,go-helper-ws-base-path ,*go-helper-current-ws* "src" "git.llnw.com" "lama"))
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
  (setenv "GOROOT" go-root))

(define-globalized-minor-mode go-helper-global-mode
  go-helper-mode
  go-helper-on)

(defun go-helper-on ()
  (go-helper-mode 1))

(provide 'go-helper-mode)
