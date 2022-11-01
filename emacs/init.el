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

(when (eq system-type 'darwin) (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))

;; font
(set-frame-font "Monaco 11" nil t)

;; graphics
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(setq make-backup-files nil)
(straight-use-package 'use-package)
(setq straight-use-package-by-default +1)
(setq display-line-numbers 'relative)
(setq scroll-conservatively most-positive-fixnum)
(setq display-line-numbers-width 3)
(setq posframe-gtk-resize-child-frames 'resize-mode)
(global-hl-line-mode +1)

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

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package which-key
  :init
  (which-key-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package evil
  :custom
  (evil-want-keybinding nil)
  :init
  (evil-mode +1))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package dashboard
  :diminish
  :custom
  (dashboard-banner-logo-title "Hejka")
  (dashboard-startup-banner 'logo)
  (dashboard-items '((projects . 10)
                     (recents . 10)))
  :init
  (dashboard-setup-startup-hook))

(use-package projectile
  :bind
  (:map projectile-mode-map
       ("C-c p" . projectile-command-map))
  :init
  (projectile-mode +1))

(use-package neotree
  :config
  (setq projectile-switch-project-action `neotree-projectile-action))

(use-package centaur-tabs
  :hook (dashboard-mode . centaur-tabs-local-mode)
  :custom
  (uniquify-separator "/")
  (centaur-tabs-style "bar")
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons t)
  (x-underline-at-descent-line t)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

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

;; notes
;; lsp-ui-flycheck-list - show errors

;; bindings
(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal 'global (kbd "<leader>nn") 'neotree-toggle)
(evil-define-key 'normal 'global (kbd "zs") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>pp") 'projectile-switch-project)

(evil-define-key 'normal 'global (kbd "<leader>h") 'windmove-left)
(evil-define-key 'normal 'global (kbd "<leader>j") 'windmove-down)
(evil-define-key 'normal 'global (kbd "<leader>k") 'windmove-up)
(evil-define-key 'normal 'global (kbd "<leader>l") 'windmove-right)
