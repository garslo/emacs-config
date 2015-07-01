;;; ginkgo-mode.el --- Helper functions for Ginkgo

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

(defgroup ginkgo nil
  "Group for configuring ginkgo minor mode"
  :group 'go)

(defcustom ginkgo-output-buffer "*ginkgo-output*"
  "Buffer to show ginkgo output"
  :type 'string
  :group 'ginkgo
  :safe 'string)

;; (regexp-opt '("It(" "Context(" "Describe("))
(defcustom ginkgo-containers-regexp "\\(?:\\(?:Context\\|Describe\\|It\\)(\\)"
  "Regexp to recognize ginkgo containers"
  :type 'string
  :group 'ginkgo
  :safe 'string)

(defcustom ginkgo-binary "ginkgo"
  "Name/location of ginkgo binary"
  :type 'string
  :group 'ginkgo
  :safe 'string)

(defvar ginkgo-test-dir ""
  "Location to run gingko tests")

(defvar ginkgo--last-focus ""
    "Holds the description of the last test that was run")

(defun ginkgo--prompt ()
  (read-file-name "Ginko dir: "))

(defun ginkgo--get-test-dir ()
  (if (string= "" ginkgo-test-dir)
	  (setq ginkgo-test-dir (ginkgo--prompt))
	ginkgo-test-dir))

(defun ginkgo-set-test-dir ()
  "Sets `ginkgo-test-dir'"
  (interactive)
  (setq ginkgo-test-dir (go-helper-prompt))
  (message "ginkgo-test-dir is %s" ginkgo-test-dir))

(defun ginkgo--run (&rest args)
  (let ((curdir default-directory))
	(message "Running \"ginkgo %s\" in dir %s" args (ginkgo--get-test-dir))
	(cd ginkgo-test-dir)
	(pop-to-buffer ginkgo-output-buffer)
	(erase-buffer)
	(other-window 1)
	(apply 'make-comint-in-buffer "ginkgo" ginkgo-output-buffer ginkgo-binary nil args)
	(cd curdir)))

(defun ginkgo-run-all ()
  (interactive)
  (ginkgo--run))

(defun ginkgo-run-this-container ()
  (interactive)
  (save-excursion
	(while (not (looking-at ginkgo-containers-regexp))
	  (backward-char))
	(let ((start nil)
		  (end nil))
	  (search-forward "\"")
	  (setq start (point))
	  (search-forward "\"")
	  (setq end (- (point) 1))
	  (let ((focus (buffer-substring-no-properties start end)))
		(setq ginkgo--last-focus focus)
		(ginkgo--run "-focus" focus)))))

(defun ginkgo-run-last ()
  (interactive)
  (if (string= "" ginkgo--last-focus)
	  (message "No focus string is stored")
	(ginkgo--run "-focus" ginkgo--last-focus)))

(defun ginkgo--make-keymap ()
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c st") 'ginkgo-set-test-dir)
	(define-key map (kbd "C-c ta") 'ginkgo-run-all)
	(define-key map (kbd "C-c tt") 'ginkgo-run-this-container)
	(define-key map (kbd "C-c tl") 'ginkgo-run-last)
	map))

(define-minor-mode ginkgo-mode
  "Minor mode for ginkgo"
  :lighter " Ginkgo"
  :keymap (ginkgo--make-keymap))

(defun ginkgo-mode-on ()
  (ginkgo-mode 1))

(provide 'ginkgo-mode)
