;; -*- lexical-binding: t; -*-
;; Provides environment variables that might me used in other modules
(defconst env-graphic-p (display-graphic-p))
(defconst env-rootp (string-equal "root" (getenv "USER")))
(defconst env-sys-mac-p (eq system-type 'darwin))
(defconst env-sys-linux-p (eq system-type 'gnu/linux))
(defconst env-sys-name (system-name))

(provide 'init-env)
