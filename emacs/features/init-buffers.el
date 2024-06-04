;; -*- lexical-binding: t; -*-
(setq display-buffer-alist
    `(;; no window
        ;; bottom side window
        ("\\*Compilation\\*"
        (display-buffer-in-side-window)
        (dedicated . t)
        (side . bottom)
        (slot . 0)
        (window-parameters . ((mode-line-format . none))))))
(provide 'init-buffers)
