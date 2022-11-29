(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(when (eq system-type 'darwin)
  (customize-set-variable 'native-comp-driver-options '("-Wl,-w"))
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (add-hook 'window-setup-hook 'toggle-frame-maximized t))

(straight-use-package 'use-package)
(setq straight-use-package-by-default +1)

;; utf8
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq auto-window-vscroll nil)

;; kll term on exit
(defadvice term-handle-exit
  (after term-kill-buffer-on-exit activate)
(kill-buffer))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; font
(when (display-graphic-p)
    (if (eq system-type 'darwin)
    (set-face-attribute 'default nil :weight 'light :font "Monaco" :height 140)
    (set-face-attribute 'default nil :weight 'bold :font "Monaco" :height 120 )))

;; ui/ux global settings
(when (eq system-type 'gnu/linux)
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(setq visible-cursor nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message (format ";; Scratch buffer - started on %s\n\n" (current-time-string)))
(setq confirm-kill-emacs 'yes-or-no-p)
(setq scroll-conservatively most-positive-fixnum)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq posframe-gtk-resize-child-frames 'resize-mode)
(setq read-process-output-max (* 1024 1024 12))
(global-hl-line-mode +1)
(setq custom-file (make-temp-file ""))
(setq split-width-threshold 9999)
(setq split-height-threshold nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq-default mode-line-format nil)
(setq enable-recursive-minibuffers t)
(setq make-backup-files nil)

(use-package dired
  :straight (:type built-in)
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))

(use-package hybrid-reverse-theme
  :config
  (load-theme 'hybrid-reverse t))

;; gc
(use-package gcmh
  :config
  (gcmh-mode +1))

;; search
(setq completion-styles '(basic substring partial-completion flex))

(use-package vertico
  :init
  (vertico-mode +1))

(use-package savehist
  :init
  (savehist-mode))

(use-package rg)

(use-package which-key
  :init
  (which-key-mode))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode +1))

(use-package evil-collection
  :after (evil)
  :init
  (evil-collection-init)
  :config
  (evil-define-key 'normal 'global (kbd "SPC") 'evil-send-leader))

(use-package projectile
  :straight (projectile :type git
			:host github
			:repo "bbatsov/projectile"
			:fork (:host github
			       :repo "mpajkowski/projectile"
			       :branch "fix/1777"))
  :init
  (setq projectile-completion-system 'default)
  :config
  (projectile-global-mode))

(use-package perspective
  :init
  (setq persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-mode))

(use-package persp-projectile
  :after (perspective))

(use-package treemacs
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-perspective
  :after (treemacs perspective)
  :config
  (treemacs-set-scope-type 'Perspectives))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package company
  :config
  (global-company-mode +1))


(use-package tree-sitter
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :init
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package eglot
  :straight (:type built-in)
  :hook
  (eglot-managed-mode . (lambda () (eldoc-mode -1)))
  :config
  (setq eglot-stay-out-of '(eldoc))
  (setq-default eglot-workspace-configuration '(:rust-analyzer (:checkOnSave (:enable t
                                                                      :command "clippy"
                                                                      :extraArgs ["--target-dir" "/tmp/rust-analyzer-check"])
                                                        :cargo (:features "all")))))

(defun flymake-clear-diagnostics ()
  "Removes diagnostics list"
  (interactive)
  (setq flymake-list-only-diagnostics '()))

(advice-add 'flymake-show-project-diagnostics :before 'flymake-clear-diagnostics)

(use-package eldoc-box)

(fset 'eldoc-doc-buffer 'eldoc-box-eglot-help-at-point)

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package vterm
  :defer t)
(use-package restclient
  :defer t)

(use-package rustic
  :defer t
  :hook
  (before-save . eglot-format-buffer)
  :mode ((rx ".rs" string-end) . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot))

(use-package vimrc-mode
  :defer t
  :mode ((rx ".vim" string-end) . vimrc-mode))

(use-package magit
  :defer t)

(use-package osm
  :defer t
  :bind (("C-c m h" . osm-home)
	 ("C-c m s" . osm-search)
	 ("C-c m v" . osm-server)
	 ("C-c m t" . osm-goto)
	 ("C-c m x" . osm-gpx-show)
	 ("C-c m j" . osm-bookmark-jump))

  :config
  (setq osm-server 'default)
  (setq osm-copyright nil))

(use-package scala-mode
  :defer t)

(use-package toml-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package json-mode
  :defer t)

(use-package zig-mode
  :defer t
  :hook
  (zig-mode . eglot-ensure))

(use-package c++-mode
  :straight (:type built-in)
  :defer t
  :hook
  (c++-mode . eglot-ensure))

(use-package c-mode
  :straight (:type built-in)
  :defer t
  :hook
  (c-mode . eglot-ensure))

(use-package csv-mode
  :defer t)

;; bindings
(evil-set-leader (list 'normal 'motion) (kbd "SPC"))
(evil-define-key 'normal 'global (kbd "zs") 'save-buffer)

(evil-define-key '(normal motion) 'global (kbd "<leader>pp") 'projectile-persp-switch-project)
(evil-define-key '(normal motion) 'global (kbd "<leader>ff") 'projectile-find-file)
(evil-define-key '(normal motion) 'global (kbd "<leader>nn") 'treemacs)
(evil-define-key '(normal motion) 'global (kbd "<leader>h") 'windmove-left)
(evil-define-key '(normal motion) 'global (kbd "<leader>j") 'windmove-down)
(evil-define-key '(normal motion) 'global (kbd "<leader>k") 'windmove-up)
(evil-define-key '(normal motion) 'global (kbd "<leader>l") 'windmove-right)

(evil-define-key 'normal 'global (kbd "TAB") 'projectile-next-project-buffer)
(evil-define-key 'normal 'global (kbd "<backtab>") 'projectile-previous-project-buffer)
(evil-define-key 'normal 'global (kbd "<leader>b") 'persp-switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>dg") 'flymake-show-project-diagnostics)
(evil-define-key 'normal 'global (kbd "<leader>rg") 'projectile-ripgrep)
(evil-define-key 'normal 'global (kbd "ga") 'eglot-code-actions)
(evil-define-key 'normal 'global (kbd "<leader>mv") 'eglot-rename)
(evil-define-key 'normal 'global (kbd "K") 'eldoc-doc-buffer)
(evil-define-key 'normal 'global (kbd "<leader>cl") 'flymake-clear-diagnostics)

(evil-define-key 'normal 'global (kbd "C-n") 'company-select-next)
(evil-define-key 'normal 'global (kbd "C-p") 'company-select-previous)

