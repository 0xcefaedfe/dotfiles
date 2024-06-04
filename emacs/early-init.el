:;-*- lexical-binding: t; -*-

;; initialize package cache
(setq package-enable-at-startup t)

;; less clutter
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
;; (add-to-list 'default-frame-alist '(internal-border-width . 18))
;; (add-to-list 'default-frame-alist '(child-frame-border-width . 2))
;; (add-to-list 'default-frame-alist '(left-fringe . 0))
;; (add-to-list 'default-frame-alist '(right-fringe . 0))

;; expect to be resized by the window manager
(setq frame-inhibit-implied-resize t)

(provide 'early-init)
