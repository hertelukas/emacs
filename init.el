;; General Configuration
;; - User Interface

;; We don't want to see a strartup message
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable visible scroll bar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room

(menu-bar-mode -1)   ; Disable menu bar

;; Font
(set-face-attribute 'default nil :font "Hack NFM" :height 100) ; TODO check if this works?

(load-theme 'tango-dark)


;; Packages
;; Initialize package sources
(require 'package) ;; Loads in package manager functionality

(setq package-archives '(("melpa". "https://melpa.org.packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents ;; If we don't have an archive of pacakges, load package archive
  (package-refresh-contents))

;; If we don't have use-package installed, do it now
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
