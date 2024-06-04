;; -*- lexical-binding: t; -*-

(use-package project
  :config
  (leader-def
    "p" '(:ignore t :wk "Project")
    "p p" '(project-switch-project :wk "Switch to project")
    "p c" '(project-compile :wk "Compile project")
    "p f" '(project-find-file :wk "Find file in project")
    "p b" '(project-switch-to-buffer :wk "Swith project buffer")))

(provide 'init-project)
