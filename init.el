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

;; Line numbers
(column-number-mode) ;; show column in modeline
(global-display-line-numbers-mode t)

;; Disable line numbers for some nodes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

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
(setq use-package-always-ensure t) ;; makes sure we downloaded it when we first use it

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
(use-package counsel
  :bind (("M-x" . counsel-M-x))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; don't start searches with ^

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; rainbow brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; which key
(use-package which-key
  :init (which-key-mode)
  :diminish  which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

;; ivy rich to get detail about commands
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Theme
(use-package catppuccin-theme
  :init (load-theme 'catppuccin :no-confirm))

;; Key bindings
;; global-set-key for global binds
;; define-key to bind something in a mode
(use-package general
  :config
  (general-create-definer lh/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  ;; Extra commands after hitting leader key
  (lh/leader-keys
    ;; Top level
    "," '(counsel-switch-buffer :which-key "Switch buffer") ; TODO maybe switch to consult
    "." '(counsel-find-file :which-key "Find file")
    "/" '(counsel-projectile-rg :which-key "Search project")
    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    "bd" '(kill-current-buffer :which-key "Kill buffer")
    "b[" '(previous-buffer :which-key "Previous buffer")
    "b]" '(next-buffer :which-key "Next buffer")
    ;; File
    "f" '(:ignore t :which-key "file")
    "ff" '(counsel-find-file :which-key "Find file")
    "fr" '(counsel-recentf :which-key "Find recent")
    "fs" '(save-buffer :which-key "Save file")
    "fS" '(write-file :which-key "Save file as...")
    ;; git
    "g" '(:ignore t :which-key "git")
    "gg" '(magit :which-key "magit-status")
    ;; org
    "n" '(:ignore t :which-key "org")
    "na" '(org-agenda :which-key "Org agenda")
    "nr" '(:ignore t :which-key "roam")
    "nrf" '(org-roam-node-find :which-key "Find node")
    "nri" '(org-roam-node-insert :which-key "Insert link")
    ;; Project
    "p" '(projectile-command-map :which-key "project")
    ;; Toggles
    "t" '(:ignore t :which-key "toggle")
    "tc" '(global-display-fill-column-indicator-mode :which-key "Column indicator")
    ;; Quit
    "q" '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-terminal :which-key "Quit emacs")
    ;; Window
    "w" '(:ignore t :which-key "window")
    "wd" '(evil-window-delete :which-key "evil-window-delete")
    "ws" '(evil-window-split :which-key "evil-window-split")
    "wv" '(evil-window-vsplit :which-key "evil-window-vsplit")
    "ww" '(evil-window-next :which-key "evil-window-next")
    "wW" '(evil-window-prev :which-key "evil-window-prev")
    )
  )

;; evil for vim
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; On line wrapping, don't jump over whole line
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;; useful for vim keybinds in other buffers which make sense
(use-package evil-collection
  :after evil ; load after evil has loaded
  :config
  (evil-collection-init))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "RET") nil))
;; TODO maybe look at hydra for cycling commands in a buffer


;; Projectile to interact with projects
(use-package projectile
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  )

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; org
(defun lh/org-mode-setup()
  (org-indent-mode)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (setq org-hide-emphasis-markers t)
  (setq org-return-follows-link t)
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  )

(use-package org
  :hook (org-mode . lh/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  ;; agenda
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-directory "~/org/")
  (setq org-agenda-files
	'("~/org/" "~/org/roam/")))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; TODO look at org-wild-notifier

;; Roam
(use-package org-roam
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      (file "~/org/roam/templates/book-template.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags:Book")
      :unnarrowed t)
     ("f" "fleeting notes" plain
      "%?"
      :target (file+head "fleet-%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags:Fleeting\n#+date:%U\n")
      :unnarrowed t)
     )
   )
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))
