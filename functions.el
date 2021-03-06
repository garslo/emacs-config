;; From http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
open and indent an empty line between the cursor and the text. Move the
cursor to the new line."
  (interactive "P")
  (electric-newline-and-maybe-indent))


;; From http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; e.g. (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
	   (package-install package)))
   packages))

(defun git-get-version ()
  (interactive)
  (insert (magit-git-string "describe" "--tags")))

(defun eval-and-replace ()
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
	  (prin1 (eval (read (current-kill 0)))
			 (current-buffer))
	(error (message "Invalid expression")
		   (insert (current-kill 0)))))

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
	(let* ((path (car element))
		   (fullpath (concat directory "/" path))
		   (isdir (car (cdr element)))
		   (ignore-dir (or (string= path ".") (string= path ".."))))
	  (cond
	   ((and (eq isdir t) (not ignore-dir))
		(load-directory fullpath))
	   ((and (eq isdir nil) (string= (substring path -3) ".el"))
		(load (file-name-sans-extension fullpath)))))))
