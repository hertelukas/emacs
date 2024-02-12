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

(load-theme 'wombat)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Packages
;; Initialize package sources
(require 'package) ;; Loads in package manager functionality

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
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

;; For auto completion in command prompt
(use-package ivy
  :bind (:map ivy-minibuffer-map ;; Apply the key bindings after only when in minibuffer map
	      ("TAB" . ivy-alt-done)
	      ("C-l" . ivy-alt-done)
	      ("C-j" . ivy-next-line) ;; vim key binds to select command
	      ("C-k" . ivy-previous-line))
  :config
  (ivy-mode))
(use-package swiper)
(use-package counsel)

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
