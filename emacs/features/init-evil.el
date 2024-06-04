;; -*- lexical-binding: t; -*-
(use-package evil
  :ensure t
  ;; on init
  :init
  ;; evil integration with various modes, required by evil-collection
  (setq evil-want-integration t) 
  ;; disable evil keybindings, required by evil-collection
  (setq evil-want-keybinding nil) 
  ;; open new vertical split windows to the right
  (setq evil-vsplit-window-right t) 
  ;; open new horizontal split windows below
  (setq evil-split-window-below t) 
  ;; don't kill the selection when pasting
  (setq evil-kill-on-visual-paste nil)
  ;; after init
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

;; evil collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; general keybindings
(use-package general
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)
  (general-create-definer leader-def
    :states '(normal visual insert emacs)
    :prefix "SPC" 
    :global-prefix "M-SPC")

  (leader-def "x" '(execute-extended-command :wk "M-x"))

  (leader-def
    "f" '(:ignore t :wk "Files")
    "f f" '(find-file :wk "Find file")
    "f r" '(recentf :wk "Find recent files")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :wk "Edit Emacs config"))

   (leader-def
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d p" '(peep-dired :wk "Peep-dired"))

  (leader-def
    "b" '(:ignore t :wk "Buffer")
    "b s" '(switch-to-buffer :wk "Switch buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b d" '(kill-this-buffer :wk "Kill this buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer"))

  (leader-def
    "e" '(:ignore t :wk "Eval")
    "e r" '(eval-region :wk "Eval region")
    "e b" '(eval-buffer :wk "Eval buffer"))

  (leader-def
    "c" '(:ignore t :wk "Code")
    "c i" '(lambda () (interactive) (indent-region (point-min) (point-max)))
    "c l" '(comment-line :wk "Comment line")
    "c r" '(comment-or-uncomment-region :wk "Comment region"))

  (leader-def
    "h" '(:ignore t :wk "Help")
    "h v" '(describe-variable :wk "Describe variable")
    "h f" '(describe-function :wk "Describe function"))

  (leader-def
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right")
    )
  )

(provide 'init-evil)
