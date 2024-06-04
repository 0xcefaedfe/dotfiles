;; -*- lexical-binding: t; -*-

(use-package company
  :commands (global-company-mode)
  :init
  (global-company-mode)
  :custom
  (company-tooltip-align-annotations 't)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1))

(provide 'init-company)
