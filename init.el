;; We don't want to see a strartup message
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable visible scroll bar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room

(menu-bar-mode -1)   ; Disable menu bar

;; Line numbers
(column-number-mode) ;; show column in modeline
(global-display-line-numbers-mode t)

;; Column indicator at 80
(setq-default display-fill-column-indicator-column 80)

;; Disable line numbers for some nodes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

;; Remember where we left off in file
(save-place-mode 1)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(set-face-attribute 'default nil :font "Hack NFM" :height 100) ; TODO check if this works?

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

(use-package consult)

(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              :map minibuffer-local-map
              ("C-w" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :after vertico
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; For better searching in minibuffers
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dashboard
  :config
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner 1)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

(use-package which-key
  :init (which-key-mode)
  :diminish  which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package catppuccin-theme
  :init (load-theme 'catppuccin :no-confirm))

(use-package projectile
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  )

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setq make-backup-files nil)

(add-hook 'find-file-hook 'recentf-save-list)

(use-package general
  :config
  (general-create-definer lh/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  ;; Extra commands after hitting leader key
  (lh/leader-keys
    ;; Top level
    "," '(consult-buffer :which-key "Switch buffer") ; TODO maybe switch to consult
    "." '(find-file :which-key "Find file")
    "/" '(consult-ripgrep :which-key "Search project")
    "RET" '(vterm-other-window :which-key "vterm")
    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    "bd" '(kill-current-buffer :which-key "Kill buffer")
    "b[" '(previous-buffer :which-key "Previous buffer")
    "b]" '(next-buffer :which-key "Next buffer")
    ;; Code
    "c" '(:ignore t :which-key "code")
    "cf" '(lsp-format-buffer :which-key "Format buffer")
    "cs" '(lsp-treemacs-symbols :which-key "Treemacs symbols")
    "cd" '(lsp-find-definition :which-key "Goto definition")
    "cr" '(lsp-rename :which-key "LSP rename")
    ;; File
    "f" '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "Find file")
    "fr" '(recentf :which-key "Find recent")
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

(use-package evil-nerd-commenter
:bind ("C-/" . evilnc-comment-or-uncomment-lines))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 10000))

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
	'("~/org/")))

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

(use-package org-roam
  :custom
  (org-roam-directory "~/org/")
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
     ("t" "todo notes" plain
      "%?"
      :target (file+head "todo-%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+filetags:TODO\n#+date:%U\n")
      :unnarrowed t)
     )
   )
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (C . T)))

(setq org-confirm-babel-evaluate nil)

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Automaticaly tangle our config.org config file when we save it
(defun lh/org-babel-tangle-config()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/Documents/Programming/emacs/config.org"))
;; Dynamic scoping to the rescue
(let ((org-confirm-babel-evaluate-nil))
(org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'lh/org-babel-tangle-config)))

(defun lh/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :hook (
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lh/lsp-mode-setup)
         )
  :commands (lsp lsp-deferred))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package lsp-treemacs
  :after lsp)

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

(add-hook 'python-mode-hook #'lsp-deferred)

(use-package rustic
  :hook (server-after-make-frame . catppuccin-reload))
