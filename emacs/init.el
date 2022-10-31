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

;; font
(set-frame-font "Monaco 11" nil t)

;; graphics
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

(use-package hybrid-reverse-theme
  :init
  (load-theme 'hybrid-reverse t))

;; search
(setq completion-styles
      '(partial-completion substring initials flex))

(setq completion-category-overrides
  '((file (styles . (partial-completion substring)))
    (buffer (styles . ( basic substring partial-completion)))
    (project-file (styles . (partial-completion substring)))
    (info-menu (styles . (substring)))))

(use-package selectrum
  :init
  (selectrum-mode +1))

(use-package rg)

(use-package gcmh
  :init
  (gcmh-mode +1))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))
(use-package which-key
  :init
  (which-key-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package evil
  :init
  (evil-mode +1))

(use-package dashboard
  :custom
  (dashboard-banner-logo-title "Hejka")
  (dashboard-startup-banner 'logo)
  (dashboard-items '((projects . 5)
		    (recents . 5)))
  :init
  (dashboard-setup-startup-hook))

(use-package projectile
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :init
  (projectile-mode +1))

(use-package treemacs
  :defer t
  :custom
  (treemacs-hide-gitignored-files-mode nil)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

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
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-suggest-server-download nil)
  :init
  :config
  (lsp-enable-which-key-integration t))

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
