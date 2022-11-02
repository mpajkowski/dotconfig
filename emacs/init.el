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
(set-frame-font "Monaco 11" nil t)

;; graphics
(when (eq system-type 'linux)
  (menu-bar-mode -1))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message (format ";; Scratch buffer - started on %s\n\n" (current-time-string)))
(setq confirm-kill-emacs 'yes-or-no-p)
(setq scroll-conservatively most-positive-fixnum)
(setq display-line-numbers-width 3)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq posframe-gtk-resize-child-frames 'resize-mode)
(setq split-width-threshold 9999)
(setq split-height-threshold nil)
(global-hl-line-mode +1)
(setq custom-file (make-temp-file ""))
(fset 'yes-or-no-p 'y-or-n-p)

(use-package hybrid-reverse-theme
  :init
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
  :custom
  (evil-want-keybinding nil)
  :init
  (evil-mode +1))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package projectile
  :bind
  (:map projectile-mode-map
       ("C-c p" . projectile-command-map))
  :init
  (projectile-mode +1))

(use-package neotree
  :custom
  (projectile-switch-project-action 'neotree-projectile-action))

(use-package perspective
  :init (persp-mode)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (defun my/persp-neo ()
   "Make NeoTree follow the perspective"
   (interactive)
   (let ((cw (selected-window))
         (path (buffer-file-name))) ;;save current window/buffer
         (progn
           (when (and (fboundp 'projectile-project-p)
                      (projectile-project-p)
                      (fboundp 'projectile-project-root))
             (neotree-dir (projectile-project-root)))
           (neotree-find path))
	 (select-window cw)))
  :hook
  (persp-switch . my/persp-neo))

(use-package persp-projectile)

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

(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0)
  :bind
  (:map company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)))

(use-package flycheck)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-suggest-server-download nil)
  ;;ui
  (lsp-eldoc-enable-hover nil)
  (lsp-signature-auto-activate nil)
  :config
  (evil-define-key 'normal 'global (kbd "gd") 'lsp-find-definition)
  (evil-define-key 'normal 'global (kbd "ga") 'lsp-execute-code-action)
  (evil-define-key 'normal 'global (kbd "<leader>mv") 'lsp-rename)
  :init
  (setq lsp-auto-execute-action nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package rustic)
(use-package toml-mode)
(use-package json-mode)

;; notes
;; lsp-ui-flycheck-list - show errors

;; bindings
(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal 'global (kbd "<leader>nn") 'neotree-toggle)
(evil-define-key 'normal 'global (kbd "zs") 'save-buffer)

(evil-define-key 'normal 'global (kbd "<leader>h") 'windmove-left)
(evil-define-key 'normal 'global (kbd "<leader>j") 'windmove-down)
(evil-define-key 'normal 'global (kbd "<leader>k") 'windmove-up)
(evil-define-key 'normal 'global (kbd "<leader>l") 'windmove-right)

(evil-define-key 'normal 'global (kbd "<leader>pp") 'projectile-persp-switch-project)
(evil-define-key 'normal 'global (kbd "TAB") 'projectile-next-project-buffer)
(evil-define-key 'normal 'global (kbd "<backtab>") 'projectile-previous-project-buffer)
(evil-define-key 'normal 'global (kbd "<leader>b") 'persp-switch-to-buffer)
