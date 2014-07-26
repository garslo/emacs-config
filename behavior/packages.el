(require 'package)
;; ========== Package Archives ========== ;;
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Auto-install these packages
(ensure-package-installed
 'markdown-mode+
 'markdown-toc
 'markdown-mode
 'git-gutter-fringe
 'fringe-helper
 'git-gutter
 'go-autocomplete
 'auto-complete
 'go-errcheck
 'go-play
 'go-projectile
 'go-eldoc
 'go-snippets
 'gotest
 'go-mode
 'f
 'icicles
 'magit
 'git-rebase-mode
 'git-commit-mode
 'paredit
 'popup
 'projectile
 'pkg-info
 'epl
 'dash
 's
 'yasnippet)

(package-initialize)
