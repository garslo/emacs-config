(defun output-of (command)
  (substring (shell-command-to-string command) 0 -1))

(defun github-link ()
  (interactive)
  (let*
	  ((git-local-filename (output-of (concat "git ls-files --full-name " (buffer-file-name))))
	   (git-branch (output-of "git rev-parse --abbrev-ref HEAD"))
	   (github-remote-url (output-of "git config --get remote.origin.url"))
	   (base (format "https://%s" (s-replace-all
								   '(("git@" . "")
									 (".git" . "")
									 (":" . "/"))
								   github-remote-url))))
	(message (format "%s/blob/%s/%s#L%s" base git-branch git-local-filename (line-number-at-pos)))))
