;; -*- lexical-binding: t; -*-
(require 'package)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(defvar prot-emacs-my-packages
  '(agitate
    altcaps
    beframe
    cursory
    denote
    dired-preview
    ef-themes
    fontaine
    lin
    logos
    mct
    modus-themes
    notmuch-indicator
    pulsar
    spacious-padding
    standard-themes
    substitute
    sxhkdrc-mode
    theme-buffet
    tmr)
  "List of symbols representing the packages I develop/maintain.")

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-pinned-packages
      `(,@(mapcar
           (lambda (package)
             (cons package "gnu-elpa-devel"))
           prot-emacs-my-packages)))

(setq custom-safe-themes t)


;; automatically install packages
(setq use-package-always-ensure t)

;; enable listing packages in a configuration file
(setq use-package-enable-imenu-support t)

;; (unless package-archive-contents
;;  (package-refresh-contents))

(use-package package-vc)

(cl-defun slot/vc-install (&key (fetcher "github") repo name rev backend)
  "Install a package from a remote if it's not already installed.
This is a thin wrapper around `package-vc-install' in order to
make non-interactive usage more ergonomic.  Takes the following
named arguments:

- FETCHER the remote where to get the package (e.g., \"gitlab\").
  If omitted, this defaults to \"github\".

- REPO should be the name of the repository (e.g.,
  \"slotThe/arXiv-citation\".

- NAME, REV, and BACKEND are as in `package-vc-install' (which
  see)."
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))

(provide 'init-use-package)
