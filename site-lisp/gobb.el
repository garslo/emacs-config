;; Not a mode yet, but some (a) helpful function
(defvar gobb-binary "go-gobb"
  "Name/location of gobb binary")

(defun gobb--run-cmd-and-insert (cmd location-fn)
  (interactive)
  (save-excursion
	(funcall location-fn)
	(insert (shell-command-to-string cmd))))

(defun gobb--move-to-end-of-file ()
  (end-of-buffer)
  (newline))

(defun gobb--run-cmd-and-insert-at-end (cmd)
  (interactive)
  (gobb--run-cmd-and-insert cmd 'gobb--move-to-end-of-file))

(defun gobb--make-cmd (&rest rest)
  (mapconcat 'identity rest " "))

(defun gobb--run-cmd-at-point (cmd)
  (gobb--run-cmd-and-insert-at-end cmd))

(defun gobb-make-interface ()
  (interactive)
  (gobb--run-cmd-at-point
   (gobb--make-cmd gobb-binary "-interface" "-unexported" "-o" (number-to-string (point)) (buffer-file-name))))

(defun gobb-make-builder ()
  (interactive)
  (gobb--run-cmd-at-point
   (gobb--make-cmd gobb-binary "-builder" "-o" (number-to-string (point)) (buffer-file-name))))

(defun gobb-make-builder-and-interface ()
  (interactive)
  (gobb--run-cmd-at-point
   (gobb--make-cmd gobb-binary "-builder" "-interface" "-o" (number-to-string (point)) (buffer-file-name))))
