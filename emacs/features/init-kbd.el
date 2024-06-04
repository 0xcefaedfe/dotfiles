;; -*- lexical-binding: t; -*-

;; define global key bindings, others are added by other modules
(use-package general :ensure t)

; which-key shows the key bindings following a prefix key
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

;; custom escape key binding hook
(defvar kbd-escape-hook nil
  "A hook run after \\[keyboard-quit] is pressed.

Triggers `kbd-escape'.

If any hook returns non-nil, all hooks after it are ignored.")

(defun kbd-escape ()
  "Run the `kbd-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop
        ;; there.
        ((cl-find-if #'funcall kbd-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

;; bind the escape key to the custom escape function
(global-set-key [remap keyboard-quit] #'kbd-escape)

(provide 'init-kbd)
