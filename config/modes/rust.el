(defun rustfmt-before-save ()
  (interactive)
  (when (eq major-mode 'rust-mode) (rustfmt-format-buffer)))

;(add-hook 'before-save-hook 'rustfmt-before-save)

;; TODO: make this not super-duper hard-coded
(setenv "RUST_SRC_PATH" "/home/garslo/dev/rust/rust_src/rustc-1.4.0/src/")
(setq racer-cmd "/home/garslo/bin/racer")
(setq racer-rust-src-path "/home/garslo/dev/rust/rust_src/rustc-1.4.0/src/")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(setq company-tooltip-align-annotations t)
(setq company-idle-delay 0.05)
;(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
