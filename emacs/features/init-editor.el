;; -*- lexical-binding: t; -*-

;; buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; indent with 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent t)
(setq-default tab-width 2)
(setq-default standard-indent 2)

;; automatic brackets and indentation
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

;; disable electric-indent mode locally
(defun editor-disable-electric-indent ()
  "Disable the command `electric-indent-mode' locally."
  (electric-indent-local-mode -1))

;; disable electric-pair mode locally
(defun editor-disable-electric-pair ()
  "Disable the command `electric-pair-mode' locally."
  (electric-pair-local-mode -1))

;; visual line move
(setq-default visual-line-mode t)

;; make sure there is a newline at the end of the file
(setq-default require-final-newline t)

;; show trailing whitespaces
(defun editor-show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'editor-show-trailing-whitespace))

;; formatting
(setq-default
 ;; `ws-butler' is used for better whitespace handling
 delete-trailing-lines nil
 sentence-end-double-space nil
 word-wrap t)

(use-package ws-butler
  :diminish
  :init
  (ws-butler-global-mode)
  :config
  (setq ws-butler-global-exempt-modes
        (append ws-butler-global-exempt-modes
                '(special-mode comint-mode term-mode eshell-mode)))
  (ws-butler-global-mode))

;; disable making backup files
(setq make-backup-files nil)

;; place auto-save files in a separate directory 
(setq-default
 auto-save-list-file-prefix (expand-file-name
                             "auto-save-list/.saves-"
                             user-emacs-directory))

;; set fill column
(setq-default fill-column 160)

(provide 'init-editor)
