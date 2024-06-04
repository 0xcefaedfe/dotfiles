;; -*- lexical-binding: t; -*-
(require 'init-env)

;; no startup  screen
(setq inhibit-startup-screen t)

;; no startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; initial buffer
(setq initial-buffer-choice nil)

;; no frame title
(setq frame-title-format nil)

;; no file dialog
(setq use-file-dialog nil)

;; no dialog box
(setq use-dialog-box nil)

;; no empty line indicators
(setq indicate-empty-lines nil)

;; no cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

(setq initial-scratch-message nil)
(setq inhibit-default-init t)

;; start easy with little dependencies to load
(setq initial-major-mode 'fundamental-mode)

;; yet keep `text-mode' as default major mode
(setq default-major-mode 'text-mode)

;; no confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; completion style, see
;; gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
(setq completion-styles '(basic substring))

;; blinking cursor
(blink-cursor-mode 1)

;; confirm before killing emacs
(setq confirm-kill-emacs #'y-or-n-p)

;; y/n instead of yes/no
(fset #'yes-or-no-p #'y-or-n-p)

;; no beeping and no blinking please
(setq ring-bell-function #'ignore)
(setq visible-bell nil)

;; make sure that trash is not drawn
(setq indicate-buffer-boundaries nil)
(setq indicate-empty-lines nil)

;; don't resize emacs in steps, it looks weird and plays bad with
;; window manager.
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; disable bidirectional text for tiny performance boost
(setq bidi-display-reordering nil)

;; size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; minimum window height
(setq window-min-height 1)

;; vertical window divider
(setq window-divider-default-right-width 18)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; no clutter, please!
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; use a nice modeline
(use-package nano-modeline
  :ensure t
  :init
  (setq-default mode-line-format nil)
  (nano-modeline-text-mode t)
  :hook
  (prog-mode           . nano-modeline-prog-mode)
  (text-mode           . nano-modeline-text-mode)
  (org-mode            . nano-modeline-org-mode)
  (pdf-view-mode       . nano-modeline-pdf-mode)
  (mu4e-headers-mode   . nano-modeline-mu4e-headers-mode)
  (mu4e-view-mode      . nano-modeline-mu4e-message-mode)
  (elfeed-show-mode    . nano-modeline-elfeed-entry-mode)
  (elfeed-search-mode  . nano-modeline-elfeed-search-mode)
  (term-mode           . nano-modeline-term-mode)
  (xwidget-webkit-mode . nano-modeline-xwidget-mode)
  (messages-buffer-mode . nano-modeline-message-mode)
  (org-capture-mode    . nano-modeline-org-capture-mode)
  (org-agenda-mode     . nano-modeline-org-agenda-mode))

(provide 'init-ui)
