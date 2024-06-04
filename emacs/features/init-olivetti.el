;; -*- lexical-binding: t; -*j
(use-package olivetti)

(defun my-enable-olivetti-mode ()
  "Enable Olivetti mode if in programming mode."
  (when (derived-mode-p 'prog-mode)
    (olivetti-set-width 160)
    (olivetti-mode 1)))

(add-hook 'prog-mode-hook #'my-enable-olivetti-mode)
(add-hook 'text-mode-hook #'my-enable-olivetti-mode)

(provide 'init-olivetti)
