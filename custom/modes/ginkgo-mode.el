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
                  (progn (message "ho") (delete-char -1))
                (progn (message "hi") (insert-char ch)))
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
  (ginkgo-toggle-container-char-prefix ?\P)
  )

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
