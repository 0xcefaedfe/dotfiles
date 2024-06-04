;; -*- lexical-binding: t; -*-

(use-package reformatter
  :ensure t)

(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\;" . swift-mode)
  :config
  (setq swift-mode:basic-offset 2)
  (require 'reformatter)
  (reformatter-define swift-format
    :program "swift-format"
    :args '("format"))
  (add-hook 'swift-mode-hook 'swift-format-on-save-mode))

(use-package eglot
  :ensure t
  :hook
  ;; Enable eglot for swift-mode
  (swift-mode . eglot-ensure)
  :config
    (fset #'jsonrpc--log-event #'ignore)
  ;; Set up sourcekit-lsp
  (add-to-list 'eglot-server-programs '(swift-mode . ("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))
  ;; Turn off breadcrumb
  (setq lsp-headerline-breadcrumb-enable nil))

(provide 'init-swift)
