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


(defun ginkgo-set-test-dir ()
  "Sets `ginkgo-test-dir' equal to the current directory"
  (interactive)
  (setq ginkgo-set-test-dir default-directory))

(defun ginkgo-run-tests ()
  (interactive)
  (let ((curdir default-directory))
	(cd ginkgo-test-dir)
	(pop-to-buffer go-ginkgo-output-buffer)
	(erase-buffer)
	(other-window 1)
	(start-process "ginkgo" go-ginkgo-output-buffer "ginkgo" "-noColor")
	(cd curdir)))

(defun ginkgo-move-backward-until-regexp-with-action (regexp action)
  (save-excursion
    (while (not (looking-at regexp))
      (backward-char))
    (funcall action)))

;; (regexp-opt '("It(" "Context(" "Describe("))
(defconst *ginkgo-containers-regexp* "\\(?:\\(?:Context\\|Describe\\|It\\)(\\)")

(defun ginkgo-toggle-container-char-prefix (ch)
  (save-excursion
    (let ((done nil))
      (while (not done)
        (if (looking-at *ginkgo-containers-regexp*)
            (progn
              (if (eq (char-before) ch)
                  (delete-char -1)
                 (insert-char ch))
              (setq done t)))
        (backward-char)))))

(defun my-chbef ()
  (interactive)
  (message (eq (char-before) ?\F)))

(defun ginkgo-toggle-container-focus ()
  (interactive)
  (ginkgo-toggle-container-char-prefix ?\F))

(defun ginkgo-toggle-container-pending ()
  (interactive)
  (ginkgo-toggle-container-char-prefix ?\P))

(defun ginkgo-make-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c g f") 'ginkgo-toggle-container-focus)
    (define-key map (kbd "C-c g p") 'ginkgo-toggle-container-pending)
    map))

(define-minor-mode ginkgo-mode
  "Helper for go interactions. We define the following bindings:

    C-c g f        ginkgo-toggle-container-focus
    C-c g p        ginkgo-toggle-container-pending"
  :lighter " Ginkgo"
  :keymap (ginkgo-make-keymap))

(provide 'ginkgo-mode)
