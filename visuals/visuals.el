;; For graphical displays only...
(when (display-graphic-p)
;; Best theme there is
(load-theme 'wombat)
;; Remove the toolbar
(tool-bar-mode -1)
;; Remove the scrollbar
(scroll-bar-mode -1)
;; Smaller font size
(set-face-attribute 'default nil :height 100) ; :height 100 => 10pt
)
;; Remove the Emacs welcome screen
(setq inhibit-splash-screen t)
;; Get faster response on keyhold
(setq echo-keystrokes 0.1)
;; Give me column numbers in the mode bar
(setq column-number-mode t)
;; Smaller tabs
(setq default-tab-width 4)
