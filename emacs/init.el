;; Brew stuff
(add-to-list 'exec-path "/opt/homebrew/bin")

;; Early initialization
(setq package-enable-at-startup nil)
(setq native-comp-async-report-warnings-errors nil)

;; Set up meta key
(cond
 ((eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))
 ((eq system-type 'gnu/linux)
  (setq x-alt-keysym 'meta)))

;; Set theme and font
(add-to-list 'default-frame-alist '(font . "Roboto Mono 14"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")

(require 'hercules-theme)

(let ((private-file "~/.emacs.d/private.el"))
  (when (file-exists-p private-file)
    (load private-file)))

;; Basic settings
(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles nil
      completion-auto-help nil
      confirm-kill-emacs 'y-or-n-p
      visible-bell nil
      package--init-file-ensured t
      custom-file "~/.emacs.d/custom.el"
      custom-safe-themes t)

(setq-default indent-tabs-mode nil
              tab-width 2)

(fido-mode 1)

;; UTF-8 everywhere
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; UI settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

;; Global keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "M-p") 'transpose-lines)
(global-set-key (kbd "M-n") (lambda () (interactive) (transpose-lines 1)))

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Use-package configuration
(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package smex
  :bind (("M-x" . smex)
         ("C-c C-c M-x" . execute-extended-command)))

;; Company mode
(use-package company
  :hook (after-init . global-company-mode))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-\""        . mc/skip-to-next-like-this)
         ("C-:"         . mc/skip-to-previous-like-this)))

;; Dired
(use-package dired
  :ensure nil
  :config
  (require 'dired-x)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")
        dired-dwim-target t
        dired-listing-switches "-alh"))

;; Tramp
(use-package tramp
  :ensure nil
  :config
  (setq tramp-auto-save-directory "/tmp"))

;; Magit
(use-package magit
  :commands magit-status)

;; Project.el
(use-package project
  :ensure nil)

;; Swift mode
(use-package swift-mode
  :mode "\\.swift\\'"
  :hook (swift-mode . swift-format-on-save-mode)
  :config
  (setq swift-mode:basic-offset 2)
  (use-package reformatter
    :config
    (reformatter-define swift-format
      :program "/opt/homebrew/bin/swift-format"
      :args '("format"))))

;; Eglot
(use-package eglot
  :ensure nil
  :hook ((swift-mode . eglot-ensure)
         (haskell-mode . eglot-ensure))
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs 
               '(swift-mode . ("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))))

(setq-default mode-line-format
              '("%e" mode-line-modified " " mode-line-buffer-identification
                "  " mode-line-position "  " mode-line-modes))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el")
  :defer t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; Load custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file))
