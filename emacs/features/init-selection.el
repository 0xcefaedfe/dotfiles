;; -*- lexical-binding: t; -*-

;; use vertico
(use-package vertico
  :hook
  (minibuffer-setup . vertico-repeat-save)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(provide 'init-selection)

