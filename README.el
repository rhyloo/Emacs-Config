(setq user-full-name "Jorge L. Benavides M."
user-real-login-name "Rhyloo"
user-mail-address "jorge2@uma.es")

(scroll-bar-mode -1)        ;; Disable visible scrollbar
(tool-bar-mode -1)          ;; Disable the toolbar
(tooltip-mode -1)           ;; Disable tooltips
(menu-bar-mode -1)          ;; Disable the menu bar
(set-fringe-mode 15)        ;; Give some breathing room (borders)
(show-paren-mode 1)         ;; Show parens
(global-hl-line-mode 1)     ;; Highlight lines

(setq org-confirm-babel-evaluate nil) ;; Stop the confirmation to evaluate org babel
(setq org-adapt-indentation t ;; Modifies paragraph filling
org-hide-leading-stars t ;; Leading stars invisible
org-odd-levels-only nil) ;; Org use only odd levels (disable)
(org-indent-mode 1)
