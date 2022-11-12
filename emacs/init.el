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
  (setq mac-command-modifier 'meta))

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

;; kll term on exit
(defadvice term-handle-exit
  (after term-kill-buffer-on-exit activate)
(kill-buffer))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; font
(if (eq system-type 'darwin)
  (set-face-attribute 'default nil :weight 'light :font "Monaco" :height 140)
  (set-face-attribute 'default nil :weight 'bold :font "Monaco" :height 120 ))

;; ui/ux global settings
(when (eq system-type 'gnu/linux)
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(setq visible-cursor nil)
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message (format ";; Scratch buffer - started on %s\n\n" (current-time-string)))
(setq confirm-kill-emacs 'yes-or-no-p)
(setq scroll-conservatively most-positive-fixnum)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq posframe-gtk-resize-child-frames 'resize-mode)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(global-hl-line-mode +1)
(setq custom-file (make-temp-file ""))
(setq split-width-threshold 9999)
(setq split-height-threshold nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)

(use-package hybrid-reverse-theme
  :config
  (load-theme 'hybrid-reverse t))

(use-package all-the-icons)

;; gc
(use-package gcmh
  :config
  (gcmh-mode +1))

;; search
(setq completion-styles '(partial-completion substring initials flex))

(setq completion-category-overrides
  '((file (styles . (partial-completion substring)))
    (buffer (styles . ( basic substring partial-completion)))
    (project-file (styles . (partial-completion substring)))
    (info-menu (styles . (substring)))))

(use-package selectrum
  :init
  (selectrum-mode +1))

(use-package rg)

(use-package which-key
  :init
  (which-key-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-persp-name              nil
        doom-modeline-buffer-encoding         nil
        doom-modeline-icon                    t
        doom-modeline-buffer-file-name-style  'truncate-with-project))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode +1))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))


(use-package projectile
  :straight (projectile :type git
			:host github
			:repo "bbatsov/projectile"
			:fork (:host github
			       :repo "mpajkowski/projectile"
			       :branch "fix/1777"))
  :bind
  (:map projectile-mode-map
       ("C-c p" . projectile-command-map))
  :init
  (setq projectile-completion-system 'default)
  :config
  (projectile-global-mode))

(use-package perspective
  :init
  (setq persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-mode))

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

(use-package persp-projectile
  :after (perspective))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package smartparens
  :config
  (progn
    (smartparens-global-mode)
    (show-smartparens-global-mode t)))

(use-package evil-smartparens
  :hook
  (smartparens-enabled . evil-smartparens-mode))

(use-package corfu
  :config
  (setq corfu-auto t)
  (setq corfu-cycle t)
  (setq corfu-auto-delay 0)
  (setq corfu-aut-prefix 0)
  (global-corfu-mode +1))

(use-package flycheck)

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  (before-save . lsp-format-buffer)
  :config
  (setq lsp-keep-workspace-alive nil)
  ;; rust-analyzer
  (setq lsp-rust-analyzer-cargo-watch-enable t)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-cargo-watch-args ["--target-dir", "/tmp/rust-analyzer-check"])
  (setq lsp-rust-analyzer-proc-macro-enable t))

(use-package lsp-ui
  :config
  (setq lsp-auto-execute-action nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-lens-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-ui-doc-position 'at-point))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))


(use-package vterm)
(use-package restclient)

(use-package rustic
  :mode ((rx ".rs" string-end) . rustic-mode)
  :config
  (require 'smartparens-rust))

(use-package vimrc-mode
  :mode ((rx ".vim" string-end) . vimrc-mode))

;;(use-package dap-mode
;;  :ensure
;;  :config
;;  (dap-ui-mode)
;;  (dap-ui-controls-mode 1)
;;
;;  (require 'dap-lldb)
;;  (require 'dap-gdb-lldb)
;;  ;; installs .extension/vscode
;;  (dap-gdb-lldb-setup)
;;  (dap-register-debug-template
;;   "Rust::LLDB Run Configuration"
;;   (list :type "lldb"
;;         :request "launch"
;;         :name "LLDB::Run"
;;	 :gdbpath "rust-lldb"
;;         :target nil
;;         :cwd nil)))

(use-package scala-mode)
(use-package toml-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package zig-mode)

;; bindings
(evil-set-leader (list 'normal 'motion) (kbd "SPC"))
(evil-define-key '(normal motion) 'global (kbd "<leader>nn") 'treemacs)
(evil-define-key 'normal 'global (kbd "zs") 'save-buffer)

(evil-define-key '(normal motion) 'global (kbd "<leader>pp") 'projectile-persp-switch-project)
(evil-define-key '(normal motion) 'global (kbd "<leader>h") 'windmove-left)
(evil-define-key '(normal motion) 'global (kbd "<leader>j") 'windmove-down)
(evil-define-key '(normal motion) 'global (kbd "<leader>k") 'windmove-up)
(evil-define-key '(normal motion) 'global (kbd "<leader>l") 'windmove-right)

(evil-define-key 'normal 'global (kbd "TAB") 'projectile-next-project-buffer)
(evil-define-key 'normal 'global (kbd "<backtab>") 'projectile-previous-project-buffer)
(evil-define-key 'normal 'global (kbd "<leader>b") 'persp-switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>dg") 'lsp-ui-flycheck-list)
(evil-define-key 'normal 'global (kbd "<leader>rg") 'projectile-ripgrep)
(evil-define-key 'normal 'global (kbd "ga") 'lsp-execute-code-action)
(evil-define-key 'normal 'global (kbd "<leader>mv") 'lsp-rename)
(evil-define-key 'normal 'global (kbd "K") 'lsp-ui-doc-glance)
