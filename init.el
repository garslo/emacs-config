
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/env.el")
(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/packages.el")
(add-hook 'after-init-hook
		  #'(lambda ()
			  (load-directory (expand-file-name "~/.emacs.d/site-lisp"))
			  (load-directory (expand-file-name "~/.emacs.d/config"))))

;; Added by emacs itself
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(ensime-ac-override-settings t)
 '(ensime-completion-style (quote auto-complete))
 '(ess-default-style (quote RStudio))
 '(flycheck-check-syntax-automatically (quote (save new-line mode-enabled)))
 '(garslo-go-tool-workspace "/usr")
 '(gh-profile-alist
   (quote
	(("github-llnw" :url "https://github.llnw.net/api/v3" :remote-regexp "^\\(?:git@github\\.llnw\\.net:\\|\\(?:git\\|https?\\|ssh\\)://.*@?github\\.llnw\\.net/\\)\\(.*\\)/\\(.*\\)\\(?:\\.git\\)?"))))
 '(gh-profile-default-profile "github-llnw")
 '(ginkgo-use-default-keys t)
 '(go-rename-command "~/bin/gorename")
 '(goflymake-debug t)
 '(goh-ws-base-dir-alist (quote ("~/dev/llnw" "~/dev/go")))
 '(goh-ws-base-dir-list (quote ("~/dev/llnw" "~/dev/go")))
 '(helm-ff-newfile-prompt-p nil)
 '(helm-truncate-lines t t)
 '(jira-url "https://jira.atlas.llnw.com/rpc/xmlrpc")
 '(package-selected-packages
   (quote
	(go-guru virtualenvwrapper virtualenv undercover toml-mode toml tern-auto-complete smart-mode-line slime salt-mode rustfmt ruby-tools ruby-dev request rails-new rails-log-mode racer protobuf-mode ponylang-mode php-mode paredit paradox ox-jira org-jira nginx-mode markdown-toc markdown-mode+ magit lua-mode key-chord julia-shell json-rpc js2-refactor jquery-doc jinja2-mode jedi helm-swoop helm-rails helm-projectile helm-project-persist helm-ls-git helm-descbinds helm-dash helm-commandlinefu helm-ag haskell-mode gotest god-mode go-scratch go-rename go-projectile go-play go-errcheck go-dlv go-direx go-autocomplete gitconfig-mode gitconfig git-gutter-fringe gist fsharp-mode forecast flycheck expand-region evil evalator-clojure ensime elein edts dot-mode dom dockerfile-mode docker django-mode django-manage discover-my-major dirtree confluence avy ac-js2 ac-cider)))
 '(paradox-github-token t)
 '(python-shell-virtualenv-path "/home/garslo/storage/dev/test"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:foreground "white"))))
 '(ensime-implicit-highlight ((t nil)))
 '(helm-selection ((t (:foreground "cyan" :underline t))))
 '(magit-blame-hash ((t (:inherit default))))
 '(magit-diff-added ((t (:background "green" :foreground "black"))))
 '(magit-diff-added-highlight ((t (:background "green" :foreground "black"))))
 '(magit-diff-base ((t (:background "#555522" :foreground "white"))))
 '(magit-diff-base-highlight ((t (:background "#666622" :foreground "white"))))
 '(magit-hash ((t (:foreground "white")))))
(put 'narrow-to-region 'disabled nil)
