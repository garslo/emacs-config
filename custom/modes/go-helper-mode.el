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

(defcustom go-helper-ws-base-path (expand-file-name "~/dev/llnw")
  "Base workspace path (directory path which contains all your *-ws dirs)"
  :type 'string
  :group 'go-helper
  :safe 'string)

(defcustom go-root "/usr/local/go"
  "GOROOT env variable"
  :type 'string
  :group 'go-helper
  :safe 'string)

(defcustom go-ginkgo-output-buffer "*ginkgo-output*"
  "Buffer to show ginkgo output"
  :type 'string
  :group 'go-helper
  :safe 'string)

(defvar *go-helper-current-ws* nil)

(defun go-helper-ws-base-as-dir ()
  (file-name-as-directory go-helper-ws-base-path))

(defun update-gopath (gopath)
  (setenv "GOPATH" (expand-file-name gopath)))

(defun go-helper-set-workspace (ws)
  "Sets the go workspace, returns the path to the workspace"
  (let* ((base-dir (go-helper-ws-base-as-dir))
         (ws-dir (concat base-dir ws "-ws")))
	(go-helper-set-workspace-raw ws-dir)
    ws-dir))

(defun go-helper-set-workspace-raw (dir)
  "Sets the workspace directly, without consulting go-helper-ws-base-path"
  (setq *go-helper-current-ws* dir)
  (update-gopath dir))

(defun go-helper-set-pwd-as-workspace ()
  "Sets pwd as the current workspace"
  (interactive)
  (go-helper-set-workspace-raw default-directory))

(defun go-helper-goto-workspace (ws)
  "Opens a dired directory in the given workspace."
  (interactive "sWorkspace name (without '-ws'): ")
  (find-file (go-helper-set-workspace ws)))

(defun go-helper-goto-repo (repo)
  "Opens a dired directory in the given repo. This requires
*go-helper-current-ws* to be set; call go-helper-goto-workspace
to set this variable."
  (interactive "sRepo name (without '.git'): ")
  (if (null *go-helper-current-ws*)
      (message "No workspace set. Call go-helper-goto-workspace.")
    (let* ((path-list `(,(getenv "GOPATH") "src" "git.llnw.com" "lama"))
           (base-dir (concat
                      (mapconcat 'file-name-as-directory path-list "")))
           (repo-name (concat repo ".git"))
           (repo-dir (mapconcat #'file-name-as-directory `(,base-dir ,repo-name) "")))
      (find-file repo-dir)
      (go-helper-set-oracle-scope))))

(defun go-helper-set-oracle-scope ()
  (interactive)
  (let* ((gopath (expand-file-name (getenv "GOPATH")))
        (repo-dir (expand-file-name default-directory))
        (current-package (go-helper-path-subtract gopath repo-dir)))
    (setq go-oracle-scope repo-dir)
    (message "Oracle scope: %s" current-package)))

(defun go-helper-set-gocode-lib-path ()
  (interactive)
  (let* ((gopath (getenv "GOPATH"))
         (command-string (format "gocode set lib-path \"%spkg/linux_amd64\"" (expand-file-name gopath))))
    (shell-command command-string)))

(defun go-helper-install-subpackages ()
  (interactive)
  (let* ((command-string "go install `go list ./...`"))
    (shell-command command-string)))

(defun go-helper-path-subtract (base child)
  (if (not (go-helper-is-subdir base child))
      (message "%s is not a subdir of %s" child base)
    (let ((start-idx (+ (length base) 4))
           (end-idx (- (length child) 1)))
      (substring child start-idx end-idx))))

(defun go-helper-is-subdir (base child)
  (let ((base-len (length base)))
    (if (>= base-len (length child))
        nil
      (equal base
         (substring child 0 base-len)))))

(defvar go-helper-test-dir ""
  "Directory where we run the ginkgo tests from")

(defun go-helper-set-test-dir ()
  "Sets `ginkgo-test-dir' equal to the current directory"
  (interactive)
  (setq go-helper-test-dir (go-helper-prompt))
  (message "go-helper-test-dir is %s" go-helper-test-dir))

(defun go-helper-get-test-dir ()
  (if (string= "" go-helper-test-dir)
	  (setq go-helper-test-dir (go-helper-prompt))
	go-helper-test-dir))

(defun go-helper-prompt ()
  (read-file-name "Ginkgo dir: "))

(defun go-helper-run-ginkgo-with-args (&rest args)
  (let ((curdir default-directory))
	(message "ginkgo args %s in dir %s" args (go-helper-get-test-dir))
	(cd go-helper-test-dir)
	(pop-to-buffer go-ginkgo-output-buffer)
	(erase-buffer)
	(other-window 1)
	(let ((proc (apply 'start-process "ginkgo" go-ginkgo-output-buffer "ginkgo" args)))
	  (set-process-filter proc (lambda (proc output)
							(with-current-buffer (process-buffer proc)
							  (save-excursion
								(goto-char (process-mark proc))
								(insert output)
								(ansi-color-apply-on-region (point-min) (point-max))
								(set-marker (process-mark proc) (point)))))))
	(cd curdir)))

;; (regexp-opt '("It(" "Context(" "Describe("))
(defconst *ginkgo-containers-regexp* "\\(?:\\(?:Context\\|Describe\\|It\\)(\\)")

(defun go-helper-run-ginkgo ()
  (interactive)
  (go-helper-run-ginkgo-with-args "-noColor"))

(defun go-helper-run-ginkgo-on-this-block ()
  (interactive)
  (save-excursion
	(while (not (looking-at *ginkgo-containers-regexp*))
	  (backward-char))
	(let ((start nil)
		  (end nil))
	 (search-forward "\"")
	 (setq start (point))
	 (search-forward "\"")
	 (setq end (- (point) 1))
	 (go-helper-run-ginkgo-with-args "-focus"  (buffer-substring-no-properties start end)))))

(defun go-helper-make-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r") 'go-helper-goto-repo)
    (define-key map (kbd "C-c w") 'go-helper-goto-workspace)
	(define-key map (kbd "C-c sw") 'go-helper-set-pwd-as-workspace)
    (define-key map (kbd "C-c ss") 'go-helper-set-oracle-scope)
    (define-key map (kbd "C-c sg") 'go-helper-set-gocode-lib-path)
    (define-key map (kbd "C-c is") 'go-helper-install-subpackages)
	(define-key map (kbd "C-c st") 'go-helper-set-test-dir)
	(define-key map (kbd "C-c C-t") 'go-helper-run-ginkgo)
	(define-key map (kbd "C-c st") 'go-helper-set-test-dir)
	(define-key map (kbd "C-c tt") 'go-helper-run-ginkgo-on-this-block)
	(define-key map (kbd "C-c ta") 'go-helper-run-ginkgo)
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
