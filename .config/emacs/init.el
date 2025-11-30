;; -*- lexical-binding: t; -*-
(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
	;;("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq ox/packages-directory (expand-file-name "packages" real-emacs-directory)
      ox/features-directory (expand-file-name "features" real-emacs-directory)
      ox/profiles-directory (expand-file-name "profiles" real-emacs-directory))

(require 'use-package)
(setq use-package-compute-statistics t
      use-package-verbose init-file-debug
      use-package-always-ensure t
      use-package-always-defer  t)

(defun ox/load (directory file-name)
  (load (expand-file-name (format "%s.el" file-name) directory)))

(defun ox/load-package (package)
  (ox/load ox/packages-directory (format "%s/%s" package package)))

(defun ox/load-feature (feature)
  (ox/load ox/features-directory feature))

(defun ox/load-profile (profile)
  (ox/load ox/profiles-directory profiles))

(ox/load-package "sane-defaults")
(ox/load-package "gcmh")
(ox/load-package "fonts")
(ox/load-package "which-key")
(ox/load-package "eat")
(ox/load-package "cua-mode")
(ox/load-package "keybindings")
(ox/load-package "org-mode")
(ox/load-package "vertico")
(ox/load-package "tab-line")
(ox/load-package "mixed-pitch")
(ox/load-package "mini-modeline")

(use-package emacs
  :custom
  custom-file (expand-file-name "custom.el" user-emacs-directory)
  :config
  (setq modus-themes-mixed-fonts t)
  (load-theme 'modus-vivendi-tinted))
