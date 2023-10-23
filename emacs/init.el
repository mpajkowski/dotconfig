(setq native-comp-speed 3)
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t)
  (setq use-package-always-ensure t))

(elpaca-wait)

(add-to-list 'display-buffer-alist
 '(me/window-bottom-left-side-window-p
   (display-buffer-in-side-window)
   (window-height . 0.30)
   (window-width . 0.55)
   (dedicated . t)
   (side . bottom)
   (slot . 0)
   (window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t)
                         (mode-line-format . 'none)))))

(defvar me/window-bottom-left-modes '())

(defun me/window-bottom-left-side-window-p (buf act)
 (with-current-buffer buf (member major-mode me/window-bottom-left-modes)))

;; mac
(when (eq system-type 'darwin)
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

(use-package general
  :config
  (general-auto-unbind-keys)
  (general-create-definer mleader-def
    :states '(normal motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC"))

(elpaca-wait)

;; font
(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(font . "Monaco-15"))
    (add-to-list 'default-frame-alist '(font . "Monego-12")))

;; global settings
(setq warning-minimum-level 'error)
(when (eq system-type 'gnu/linux)
  (menu-bar-mode -1))
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(global-auto-revert-mode t)
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
(setq make-backup-files nil)
(setq auto-revert-verbose nil)
(setq column-number-mode t)
(setq split-width-threshold 9999)
(setq split-height-threshold nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq scroll-preserve-screen-position 'always)
(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold (* 100 1000 1000))))
(add-hook 'focus-out-hook 'garbage-collect)
(run-with-idle-timer 5 t 'garbage-collect)

(use-package hl-line
  :elpaca nil
  :ensure nil
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil))

(modify-syntax-entry ?_ "w")

(use-package orderless
  :init
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :elpaca (vertico :type git
                   :host github
                   :repo "minad/vertico"
                   :files (:defaults "extensions/*"))
  :init
  (vertico-mode +1))

(use-package consult
  :demand t
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

(use-package consult-project-extra
  :after (consult)
  :config
  (setq consult-project-extra-sources
        (list consult-project-extra--source-buffer
              consult-project-extra--source-file)))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode))

(use-package project
  :elpaca nil
  :ensure nil
  :after (consult)
  :config
  (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep" "r") t))

(use-package savehist
  :elpaca nil
  :ensure nil
  :init
  (savehist-mode))

(use-package uniquify
  :elpaca nil
  :ensure nil
  :config
  (setq uniquify-separator "/"
        uniquify-buffer-name-style 'forward))

(use-package buffer-name-relative
  :init
  (buffer-name-relative-mode))

(use-package which-key
  :init
  (which-key-mode))

(use-package project-tab-groups
  :init
  (setq tab-bar-format '(tab-bar-format-history tab-bar-format-tabs-groups tab-bar-separator tab-bar-format-add-tab))
  (project-tab-groups-mode 1))


(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-icon t
        doom-modeline-lsp t
        doom-modeline-buffer-file-name-style  'truncate-with-project))

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode +1))

(use-package evil-collection
  :after (evil)
  :config
  (setq evil-collection-key-blacklist '("SPC"))
  (evil-collection-init))

(use-package dired
  :ensure nil
  :elpaca nil
  :demand t
  :after (evil evil-collection)
  :config
  (general-def 'normal dired-mode-map "h" 'dired-up-directory)
  (general-def 'normal dired-mode-map "l" 'dired-find-file)
  (setf dired-kill-when-opening-new-dired-buffer t))

(use-package nerd-icons
  :demand t)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(use-package evil-numbers
  :after (evil evil-collection)
  :config
  (general-def '(normal visual) "C-c =" 'evil-numbers/inc-at-pt)
  (general-def '(normal visual) "C-c -" 'evil-numbers/dec-at-pt))

(use-package doom-themes
  :after (nerd-icons)
  :config
  (setq doom-themes-enable-bold t
        doom-theme-enable-italic t)
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-org-config))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package treesit
  :elpaca nil
  :ensure nil
  :config
  (setq treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (c . ("https://github.com/tree-sitter/tree-sitter-c"))
     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
     (css . ("https://github.com/tree-sitter/tree-sitter-css"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (html . ("https://github.com/tree-sitter/tree-sitter-html"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
     (make . ("https://github.com/alemuller/tree-sitter-make"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (php . ("https://github.com/tree-sitter/tree-sitter-php"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src" "typescript"))
     (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
     (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
     (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
     (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
     (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  (defun me/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))


(use-package corfu
  :config
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.3)
  (setq corfu-preview-current nil)
  (setq corfu-preselect nil)
  (global-corfu-mode)
  (corfu-echo-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package eglot
  :elpaca nil
  :ensure nil
  :hook
  (eglot-managed-mode . (lambda ()
                          (eglot-inlay-hints-mode -1)
                          (setq-local completion-at-point-functions
                                      (list
                                       (cape-super-capf
                                        #'eglot-completion-at-point
                                        #'cape-keyword)))))
  (before-save . eglot-format-buffer)
  :init
  (setq-default eglot-workspace-configuration
                '(:rust-analyzer (:checkOnSave (:enable t
                                                :command "clippy"
                                                :extraArgs ["--target-dir" "/tmp/rust-analyzer-check" "--" "-D" "warnings"])
                                  :cargo (:features "all")
                                  :check (:allTargets :json-false)))))
(use-package eldoc
  :elpaca nil
  :ensure nil
  :config
  (setq eldoc-echo-area-use-multiline-p nil))


(use-package flymake
  :elpaca nil
  :ensure nil
  :after (evil)
  :init
  ;(add-to-list 'me/window-bottom-left-modes 'flymake-project-diagnostics-mode)
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
  :after (evil evil-collection)
  :elpaca (vterm :post-build
                 (progn
                   (setq vterm-always-compile-module t)
                   (require 'vterm)
                   ;;print compilation info for elpaca
                   (with-current-buffer (get-buffer-create vterm-install-buffer-name)
                     (goto-char (point-min))
                     (while (not (eobp))
                       (message "%S"
                                (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                       (forward-line)))
                   (when-let ((so (expand-file-name "./vterm-module.so"))
                              ((file-exists-p so)))
                     (make-symbolic-link
                      so (expand-file-name (file-name-nondirectory so)
                                           "../../builds/vterm")
                      'ok-if-already-exists))))
  :init
  (add-to-list 'me/window-bottom-left-modes 'vterm-mode)
  :config
  (defun evil-collection-vterm-append ()
    "Append character after cursor. Fixes evil collection"
    (interactive)
    (vterm-goto-char (1+ (point)))
    (call-interactively #'evil-append))
  (setq vterm-timer-delay 0.05))

(use-package multi-vterm
  :after (vterm))

(use-package restclient
  :defer t)

(use-package magit
  :defer t)

(use-package magit-delta
  :defer t
  :hook
  (magit-mode . magit-delta-mode))

;; languages
(use-package rust-ts-mode
  :elpaca nil
  :ensure nil
  :defer t
  :mode ((rx ".rs" string-end) . rust-ts-mode)
  :hook
  (rust-ts-mode . (lambda () (eglot-ensure) (cargo-minor-mode))))

(use-package cargo
  :defer t)

(use-package toml-ts-mode
  :elpaca nil
  :ensure nil
  :defer t
  :mode ("\\(?:Dockerfile\\|dockerfile\\)\\(?:\\..*\\)?" . dockerfile-ts-mode))

(use-package dockerfile-ts-mode
  :elpaca nil
  :ensure nil
  :defer t
  :mode ((rx "Dockerfile" string-end) . dockerfile-ts-mode))

(use-package vimrc-mode
  :defer t
  :mode ((rx ".vim" string-end) . vimrc-mode))

(use-package markdown-mode
  :defer t)

(use-package scala-mode
  :defer t
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :defer t
  :after (scala-mode)
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package yaml-mode
  :defer t)

(use-package json-mode
  :defer t)

(use-package zig-mode
  :defer t
  :hook
  (zig-mode . eglot-ensure))

(use-package c++-mode
  :elpaca nil
  :ensure nil
  :defer t
  :hook
  (c++-mode . eglot-ensure))

(use-package c-mode
  :elpaca nil
  :ensure nil
  :defer t
  :hook
  (c-mode . eglot-ensure))

(use-package csv-mode
  :defer t)

(use-package kubel
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
  (setq osm-copyright t))


(elpaca-wait)

;; bindings
(general-def 'normal 'global "zs" 'save-buffer)

(mleader-def '(normal motion emacs) 'global "pp" 'project-switch-project)
(mleader-def '(normal motion emacs) 'global "eb" 'eval-buffer)
(mleader-def '(normal motion emacs) 'global "er" 'eval-region)
(mleader-def '(normal motion emacs) 'global "pf" 'consult-project-extra-find)
(mleader-def '(normal motion emacs) 'global "b" 'consult-project-buffer)
(mleader-def '(normal motion emacs) 'global "ok" 'kubel)
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
(mleader-def '(normal motion emacs) 'global "cl" 'me/flymake-clear-diagnostics)

(message "hejka")
