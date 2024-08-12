;; set up meta key
(cond
 ((eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))
 ((eq system-type 'gnu/linux)
  (setq x-alt-keysym 'meta)))

;; set theme early
(add-to-list 'default-frame-alist '(font . "RobotoMono NerdFont"))
(add-to-list 'custom-theme-load-path ".")
(load-theme 'gruber-darker t)

;; silence native compilation warnings
(setq native-comp-async-report-warnings-errors nil)

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq completion-auto-help nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq visible-bell nil)
(setq package--init-file-ensured t)
(setq custom-file "~/.emacs.d/custom.el")
(setq custom-safe-themes t)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; turn some modes on 
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(delete-selection-mode)
(global-auto-revert-mode t)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; eye candy
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

;; function definitions
(defvar nxt/package-contents-refreshed nil)

(defun nxt/package-refresh-contents-once ()
  (when (not nxt/package-contents-refreshed)
    (setq nxt/package-contents-refreshed t)
    (package-refresh-contents)))

(defun nxt/require-one-package (package)
  (when (not (package-installed-p package))
    (nxt/package-refresh-contents-once)
    (package-install package)))

(defun nxt/require (&rest packages)
  (dolist (package packages)
    (nxt/require-one-package package)))

;; initialize package management
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; ido and smex
(nxt/require 'smex 'ido-completing-read+)
(require 'ido-completing-read+)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

;; compile
(require 'compile)
(global-set-key (kbd "C-c c") 'compile)

;; move text
(nxt/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; company
(nxt/require 'company)
(require 'company)
(global-company-mode)

;; multiple cursors
(nxt/require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; dired
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")

;; tramp
(setq tramp-auto-save-directory "/tmp")

;; magit
(nxt/require 'magit)

;; project.el
(nxt/require 'project)

(nxt/require 'swift-mode)
(nxt/require 'reformatter)

;; Swift mode
(use-package swift-mode
  :mode ("\\.swift\\;" . swift-mode)
  :config
  (setq swift-mode:basic-offset 2)
  (require 'reformatter)
  (reformatter-define swift-format
    :program "swift-format"
    :args '("format"))
  (add-hook 'swift-mode-hook 'swift-format-on-save-mode))

;; LSP
(use-package eglot
  :hook
  ;; Enable eglot for swift-mode
  (swift-mode . eglot-ensure)
  :config
    (fset #'jsonrpc--log-event #'ignore)
  ;; Set up sourcekit-lsp
  (add-to-list 'eglot-server-programs '(swift-mode . ("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))
  ;; Turn off breadcrumb
  (setq lsp-headerline-breadcrumb-enable nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" default))
 '(package-selected-packages '(magit ido-completing-read+ smex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
