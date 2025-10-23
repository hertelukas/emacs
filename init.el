;; ---------------------------
;; Package managmenet: straight.el
;; --------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use by default
(setq straight-use-package-by-default t)
;; ---------------------------
;; General settings
;; ---------------------------
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Line numbers
(global-hl-line-mode t)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Remember where we left off in files
(save-place-mode 1)

;; Disable startup message
(setq inhibit-startup-message t)

;; If file changes on disk and no unsaved changes, update
(global-auto-revert-mode 1)

;; Disable autosave and lock files
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq make-backup-files nil)

;; Show column in modeline
(column-number-mode 1)

;; Auto close brackets
(electric-pair-mode 1)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))
;; ----------------------
;; Window management
;; ----------------------
(defvar window-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "w") #'other-window)
    (define-key keymap (kbd "u") #'winner-undo)
    (define-key keymap (kbd "r") #'winner-redo)
    (define-key keymap (kbd "h") #'split-window-below)
    (define-key keymap (kbd "v") #'split-window-right)
    (define-key keymap (kbd "c") #'delete-window)
    (define-key keymap (kbd "o") #'delete-other-windows)
    keymap))

;; define an alias for your keymap
(defalias 'window-keymap window-keymap)
(winner-mode 1)
;; ---------------------------
;; Modal editing: Meow
;; ---------------------------
(defvar file-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "r") #'recentf)
    (define-key keymap (kbd "s") #'save-buffer)
    (define-key keymap (kbd "f") #'find-file)
    keymap))

(defalias 'file-keymap file-keymap)

(defvar eglot-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "a") #'eglot-code-actions)
    (define-key keymap (kbd "f") #'eglot-format-buffer)
    (define-key keymap (kbd "D") #'eglot-find-typeDefinition)
    (define-key keymap (kbd "d") #'eglot-find-declaration)
    (define-key keymap (kbd "i") #'eglot-find-implementation)
    keymap))

(defalias 'eglot-keymap eglot-keymap)

(defvar roam-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "f") #'org-roam-node-find)
    (define-key keymap (kbd "i") #'org-roam-node-insert)
    keymap))

(defalias 'roam-keymap roam-keymap)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("," . consult-buffer)
   '("." . find-file)
   '("w" . window-keymap)
   '("f" . file-keymap)
   '("e" . eglot-keymap)
   '("r" . roam-keymap)
   ;; Project options
   '("p" . "C-x p")
   ;; Quit options
   '("q q" . save-buffers-kill-terminal)
   '("/" . consult-ripgrep)
   '("?" . meow-cheatsheet)
   )
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :init
  (setq meow-use-clipboard t)
  :config
  (meow-setup)
  (meow-global-mode 1)

  (add-hook 'meow-insert-enter-hook
	    (lambda ()
	      (unless (derived-mode-p 'org-mode)
		(setq display-line-numbers t))))
  

  (add-hook 'meow-insert-exit-hook
	    (lambda ()
	      (unless (derived-mode-p 'org-mode)
	      (setq display-line-numbers 'relative)))))

;; tab completes
(setq tab-always-indent 'complete)

;; ----------------------
;; Color Theme
;; ----------------------
(straight-use-package 'catppuccin-theme)
(load-theme 'catppuccin :no-confirm)

;; ----------------------
;; Magit
;; ----------------------
(straight-use-package 'magit)

;; ----------------------
;; LSP
;; ----------------------
(straight-use-package 'eglot)

(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust")))

(unless (treesit-language-available-p 'rust)
  (treesit-install-language-grammar 'rust))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook #'eglot-ensure)
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

;; ----------------------
;; Completion
;; ----------------------
(use-package vertico
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      :map minibuffer-local-map
	      ("C-w" . backward-kill-sexp))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt) ;; Always preselect the prompt

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
	("C-j" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ("C-k" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package yasnippet
  :config
  (yas-global-mode  1))

(use-package yasnippet-snippets
  :after yasnippet)

(setq flymake-show-diagnostics-at-end-of-line 1)

(use-package flymake
  :config
  (setq flymake-error-bitmap   '(vertical-bar compilation-error)
        flymake-warning-bitmap '(vertical-bar compilation-warning)
        flymake-note-bitmap    '(vertical-bar compilation-info)))

;; ----------------------
;; Org Mode
;; ----------------------
(defun lh/org-mode-setup()
  (org-indent-mode 1)
  (visual-line-mode 1)
  (display-line-numbers-mode -1)
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

(defun lh/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.5)
  (set-face-attribute 'org-level-1 nil :height 1.5)
  (set-face-attribute 'org-level-2 nil :height 1.4)
  (set-face-attribute 'org-level-3 nil :height 1.3)
  (set-face-attribute 'org-level-4 nil :height 1.2)
  (set-face-attribute 'org-level-5 nil :height 1.1))

(use-package visual-fill-column
  :hook (org-mode . lh/org-mode-visual-fill))

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

(use-package org-appear
  :hook (org-mode . org-appear-mode))
