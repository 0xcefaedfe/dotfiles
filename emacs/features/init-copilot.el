;; -*- lexical-binding: t; -*-
(use-package copilot
  :init (slot/vc-install :fetcher "github" :repo "copilot-emacs/copilot.el")
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (setq copilot-indent-offset-warning-disable t))

(provide 'init-copilot)
