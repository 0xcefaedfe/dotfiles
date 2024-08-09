;; set theme early
(add-to-list 'default-frame-alist '(font . "RobotoMono NerdFont"))
(add-to-list 'custom-theme-load-path ".")
(load-theme 'gruber-darker t)

;; silence native compilation warnings
(setq native-comp-async-report-warnings-errors nil)

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
