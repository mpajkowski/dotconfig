;; initialize straight
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

;; mac
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
(setq auto-window-vscroll nil)

;; kill term on exit
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
    (set-face-attribute 'default nil :weight 'normal :font "Monaco" :height 140 )))

;; ui/ux global settings
(when (eq system-type 'gnu/linux)
  (menu-bar-mode -1))
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
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
(setq custom-file (make-temp-file ""))
(setq split-width-threshold 9999)
(setq split-height-threshold nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq enable-recursive-minibuffers t)
(setq make-backup-files nil)
(setq delete-auto-save-files t)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)
(setq auto-revert-verbose nil)
(setq column-number-mode t)

(global-hl-line-mode)

(modify-syntax-entry ?_ "w")

;; autosave
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

(use-package dired
  :straight (:type built-in)
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))

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
  :straight (:type built-in)
  :init
  (savehist-mode))

(use-package uniquify
  :straight (:type built-in)
  :config
  (setq uniquify-separator "/"
        uniquify-buffer-name-style 'forward))

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
  (setq evil-want-integration t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode +1))

(use-package evil-numbers
  :after (evil)
  :config
  (evil-define-key '(normal visual) 'global (kbd "C-c =") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

(use-package corfu
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 1
        corfu-preselect-first nil)
  (global-corfu-mode +1))

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

(use-package all-the-icons)

(use-package doom-themes
  :after (treemacs all-the-icons)
  :config
  (setq doom-themes-enable-bold nil
        doom-theme-enable-italic nil)
  (load-theme 'doom-tomorrow-night t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package treesit
  :straight (:type built-in)
  :init
  (setq treesit-extra-load-path '("/usr/local/lib")))

(use-package eglot
  :straight (:type built-in)
  :hook
  (eglot-managed-mode . (lambda () (eldoc-mode -1)))
  :config
  (setq eglot-stay-out-of '(eldoc))
  (setq-default eglot-workspace-configuration
                '(:rust-analyzer (:checkOnSave (:enable t
                                                :command "clippy"
                                                :extraArgs ["--target-dir" "/tmp/rust-analyzer-check"])
                                  :cargo (:features "all")))))

(use-package eldoc-box)

(fset 'eldoc-doc-buffer 'eldoc-box-eglot-help-at-point)

(defun flymake-clear-diagnostics ()
  "Removes diagnostics list"
  (interactive)
  (setq flymake-list-only-diagnostics '()))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package vterm
  :defer t
  :config
  (add-hook 'vterm-mode-hook 'evil-emacs-state))

(use-package multi-vterm
  :defer t
  :config
  (add-hook 'vterm-mode-hook
                  (lambda ()
                  (setq-local evil-insert-state-cursor 'box)
                  (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(use-package restclient
  :defer t)

(use-package rustic
  :defer t
  :hook
  (before-save . eglot-format-buffer)
  (rustic-mode . eglot-ensure)
  :config
  (setq rustic-default-test-arguments "--benches --tests --all-features -- --nocapture")
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
(evil-define-key '(normal motion) 'global (kbd "<leader>tt") 'multi-vterm-project)
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

