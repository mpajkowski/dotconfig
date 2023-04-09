(setq straight-repository-branch "develop")
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

(straight-use-package 'use-package)
(setq use-package-always-ensure t)
(setq straight-use-package-by-default t)

;; mac
(when (eq system-type 'darwin)
  (customize-set-variable 'native-comp-driver-options '("-Wl,-w"))
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta))

;; utf8
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq auto-window-vscroll nil)

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package no-littering
  :config
   (setq no-littering-etc-directory
	(expand-file-name "config/" user-emacs-directory))
   (setq no-littering-var-directory
	(expand-file-name "data/" user-emacs-directory))
   (make-directory (no-littering-expand-var-file-name "auto-save/") t)
   (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; font
(when (display-graphic-p)
  (if (eq system-type 'darwin)
      ((set-face-attribute 'default nil :weight 'normal :font "Monaco" :height 150)
       (display-battery-mode +1))
    (set-face-attribute 'default nil :weight 'normal :font "Monaco" :height 110)))

;; global settings
(setq warning-minimum-level 'error)
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
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq enable-recursive-minibuffers t)
(setq create-lockfiles nil)
(setq auto-revert-verbose nil)
(setq column-number-mode t)
(setq split-width-threshold 9999)
(setq split-height-threshold nil)

(use-package hl-line
  :straight (:type built-in)
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil))

(modify-syntax-entry ?_ "w")

(use-package gcmh
  :config
  (gcmh-mode +1))

(use-package general
  :config
  (general-auto-unbind-keys)
  (general-create-definer mleader-def
    :states '(normal motion emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"))

(use-package orderless
  :init
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                ;; vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :init
  (vertico-mode +1))

(if window-system
    (use-package vertico-posframe
    :config
    (vertico-posframe-mode +1)))

(use-package consult
  :bind (
         ("C-c M-x" . consult-mode-command)
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  (setq consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS")
  (setq consult-narrow-key "<"))

(use-package savehist
  :init
  (savehist-mode))

(use-package uniquify
  :straight (:type built-in)
  :config
  (setq uniquify-separator "/"
        uniquify-buffer-name-style 'forward))

(use-package which-key
  :init
  (which-key-mode))

(use-package tabspaces
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :init
  (tabspaces-mode +1)
  :config
  (setq tabspaces-use-filtered-buffers-as-default t
        tabspaces-default-tab "Default"
        tabspaces-remove-to-default t
        tabspaces-include-buffers '("*scratch*")
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-new-tab-to "rightmost")
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                          :predicate #'tabspaces--local-buffer-p
                          :sort 'visibility
                          :as #'buffer-name)))
    "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))
  (defun me/tabspaces-switch-project ()
    (interactive)
     (call-interactively 'tabspaces-open-or-create-project-and-workspace)
     (let ((scratch (get-buffer "*scratch*")))
       (tabspaces-remove-selected-buffer scratch))))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-persp-name              nil
        doom-modeline-buffer-encoding         nil
        doom-modeline-icon                    t
        doom-modeline-buffer-file-name-style  'truncate-with-project))


(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode +1))

(use-package evil-numbers
  :after (evil)
  :config
  (general-def '(normal visual) "C-c =" 'evil-numbers/inc-at-pt)
  (general-def '(normal visual) "C-c -" 'evil-numbers/dec-at-pt))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

(use-package company
  :config
  (global-company-mode +1))

(use-package all-the-icons)

(use-package doom-themes
  :after (all-the-icons)
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
  (setq treesit-extra-load-path '("/usr/local/lib" "~/.config/treesit")))

(use-package eglot
  :straight (:type built-in)
  :hook
  (eglot-managed-mode . (lambda () (eldoc-mode -1) (eglot-inlay-hints-mode -1)))
  :config
  (setq eglot-stay-out-of '(eldoc))
  (setq-default eglot-workspace-configuration
                '(:rust-analyzer (:checkOnSave (:enable t
                                                :command "clippy"
                                                :extraArgs ["--target-dir" "/tmp/rust-analyzer-check" "--" "-D" "warnings"])
                                  :cargo (:features "all")))))

(use-package eldoc-box)

(fset 'eldoc-doc-buffer 'eldoc-box-eglot-help-at-point)

(use-package flymake
  :straight (:type built-in)
  :config
  (evil-make-overriding-map flymake-project-diagnostics-mode-map 'normal)
  (general-def 'normal flymake-project-diagnostics-mode-map "q" 'kill-buffer-and-window)
  (general-def 'normal flymake-project-diagnostics-mode-map "ZZ" 'kill-buffer-and-window)
  (defun me/flymake-clear-diagnostics ()
    "Removes diagnostics list"
    (interactive)
    (setq flymake-list-only-diagnostics '())))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package vterm
  :defer t)

(use-package multi-vterm
  :defer t
  :config
  (setq multi-vterm-dedicated-window-height-percent 30))


(use-package restclient
  :defer t)

(use-package rustic
  :defer t
  :hook
  (before-save . eglot-format-buffer)
  (rustic-mode . eglot-ensure)
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-default-test-arguments "--benches --tests --all-features -- --nocapture"))

(use-package vimrc-mode
  :defer t
  :mode ((rx ".vim" string-end) . vimrc-mode))

(use-package magit
  :defer t)

(use-package magit-delta
  :defer t
  :hook
  (magit-mode . magit-delta-mode))

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
(general-def 'normal 'global "zs" 'save-buffer)

(mleader-def '(normal motion emacs) 'global "pp" 'me/tabspaces-switch-project)
(mleader-def '(normal motion emacs) 'global "pf" 'project-find-file)
(mleader-def '(normal motion emacs) 'global "b" 'consult-buffer)
(mleader-def '(normal motion emacs) 'global "nn" 'treemacs)
(mleader-def '(normal motion emacs) 'global "tt" 'multi-vterm-project)
(mleader-def '(normal motion emacs) 'global "h" 'windmove-left)
(mleader-def '(normal motion emacs) 'global "j" 'windmove-down)
(mleader-def '(normal motion emacs) 'global "k" 'windmove-up)
(mleader-def '(normal motion emacs) 'global "l" 'windmove-right)

(mleader-def '(normal motion emacs) 'global "dg" 'flymake-show-project-diagnostics)
(mleader-def '(normal motion emacs) 'global "rg" 'consult-ripgrep)
(general-def 'normal 'global "ga" 'eglot-code-actions)
(mleader-def 'normal 'global "mv" 'eglot-rename)
(general-def 'normal 'global "K" 'eldoc-doc-buffer)
(mleader-def '(normal motion emacs) 'global "cl" 'me/flymake-clear-diagnostics)
