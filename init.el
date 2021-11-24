(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Instalando o use-package se não estiver instalado
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package no-littering)

;; Arquivos de auto-save no var
(setq auto-save-file-name-transforms
 `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(menu-bar-mode -1)
;; (scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq default-frame-alist '((undecorated . t)
			    (font . "Source Code Pro-12")))

(add-hook 'server-after-make-frame-hook #'toggle-frame-maximized t)
(add-hook 'server-after-make-frame-hook #'(lambda () (scroll-bar-mode -1)))

(setq inhibit-startup-screen t)

(set-fringe-mode 10)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(dolist (mode '(term-mode-hook
		dired-mode-hook
		calendar-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package smooth-scrolling
  :init (smooth-scrolling-mode 1))

(use-package doom-themes)
(load-theme 'doom-one t)

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-major-mode-icon nil))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :config (evilnc-default-hotkeys t) ;; use default key bindings (M-;) in Emacs state
  :bind (:map evil-normal-state-map
	      ("gc" . evilnc-comment-or-uncomment-lines)))

(defun ed/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
	  (zap-up-to-char (- arg) ?/)
	(delete-minibuffer-contents))
    (backward-delete-char arg)))

(use-package vertico
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      ("C-l" . vertico-exit-input)
	      :map minibuffer-local-map
	      ("M-h" . backward-kill-word)
	      ("<backspace>" . ed/minibuffer-backward-kill))
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(general-def 'normal 'dired-mode-map
  "h" #'dired-up-directory
  "l" #'dired-find-file)

(setq dired-listing-switches "-al --group-directories-first")

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-strict-mode))

(defun ed/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . ed/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-eldoc-enable-hover nil)
  (lsp-lens-enable nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-signature-render-documentation nil)
  (lsp-ui-doc-show-with-cursor nil))

(use-package python-mode)
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp-deferred))))

(use-package which-key
  :defer 0
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package general :after evil)

(general-create-definer ed/leader-key
  :states '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-c")

(ed/leader-key
  "SPC" #'find-file
  "."   #'dired-jump
  ":"   #'execute-extended-command
  "c"   #'compile
  "b"   #'switch-to-buffer
  "w"     evil-window-map
  "h"     help-map)

;; Automatically tangle our Emacs.org config file when we save it
(defun ed/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
		      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ed/org-babel-tangle-config)))

(setq gc-cons-threshold (* 2 1000 1000))
