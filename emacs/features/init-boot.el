;; -*- lexical-binding: t; -*-

;; turn off native compilation warnings
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; increase the garbage collection threshold during init
;; after init, reset it to a lower value
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold
                             normal-gc-cons-threshold))))

;; use the newer version of a compiled file
(setq-default load-prefer-newer t)

;; save custom settings in a separate file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; load the custom settings file
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;; automatically save session on quit
(setq desktop-save t)
(desktop-save-mode 1)
(desktop-read)

(provide 'init-boot)
