;; -*- lexical-binding: t; -*-

(use-package magit
  :config
  (leader-def
    "g" '(:ignore t :wk "Magit")
    "g s" '(magit-status :wk "Magit status")))

(provide 'init-magit)
