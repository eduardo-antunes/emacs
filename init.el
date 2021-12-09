(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
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
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(add-hook 'server-after-make-frame-hook #'(lambda () (scroll-bar-mode -1)
                                            (set-frame-font "Source Code Pro-12")))

(setq inhibit-startup-screen t)

(set-fringe-mode 10)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
                term-mode-hook
                calendar-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package smooth-scrolling
  :init (smooth-scrolling-mode 1))

(use-package doom-themes)
(load-theme 'doom-one t)
(set-frame-font "Source Code Pro-12" nil t)

(use-package all-the-icons)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (size-indication-mode 1)
  :custom
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project))

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
  "Quando o minibuffer estiver completando o nome de um arquivo, delete tudo até a sua pasta-pai; do contrário, delete normalmente"
  (interactive "p")
  (if minibuffer-completing-file-name
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

(use-package corfu
  :demand t
  :bind (:map corfu-map
              ("M-j" . corfu-next)
              ("M-k" . corfu-previous))
  :custom
  (corfu-cycle t)
  :config
  (setq tab-always-indent 'complete)
  (corfu-global-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(defun ed/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . ed/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun ed/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ed/org-mode-visual-fill))

(general-def 'normal 'dired-mode-map
  "h" #'dired-up-directory
  "l" #'dired-find-file)

(setq dired-listing-switches "-al --group-directories-first")

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-enable-snippet nil)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-which-key-integration))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;; lsp-ui documentation panels
  (lsp-ui-doc-max-height 8)
  (lsp-ui-doc-max-width 72)
  (lsp-ui-doc-delay 0.75)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'at-point)

  ;; lsp-ui sideline
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)

  ;; lsp-ui miscelaneous
  (lsp-lens-enable t)
  (lsp-signature-render-documentation nil))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package projectile
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(defun ed/c-cpp-mode-setup ()
  (c-set-style "cc-mode")
  (c-toggle-auto-state 1)
  (lsp-deferred))

(add-hook 'c-mode-hook   #'ed/c-cpp-mode-setup)
(add-hook 'c++-mode-hook #'ed/c-cpp-mode-setup)

(use-package nasm-mode
  :mode "\\.asm\\'")

(use-package python-mode)
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package dart-mode)

(use-package lsp-dart
  :hook (dart-mode . lsp-deferred)
  :custom
  (lsp-dart-flutter-sdk-dir "~/Downloads/flutter")
  (lsp-dart-sdk-dir (concat lsp-dart-flutter-sdk-dir "/bin/cache/dart-sdk")))

(use-package flutter
  :after dart-mode
  :general
  (ed/leader-key
    "mr" '(flutter-run-or-hot-reload :which-key "hot reload")))

(use-package general :after evil)

(use-package which-key
  :defer 0
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(general-create-definer ed/leader-key
  :states '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-c")

(ed/leader-key
  "SPC" #'find-file
  "."   #'dired
  ":"   '(execute-extended-command :which-key "M-x")
  "c"   '(:ignore t :which-key "compile")
  "cc"  '(compile)
  "cr"  '(recompile)
  "b"   #'switch-to-buffer
  "w"   '(:keymap evil-window-map :which-key "window")
  "h"   '(:keymap help-map :which-key "help")

;; Automatically tangle our Emacs.org config file when we save it
(defun ed/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ed/org-babel-tangle-config)))

(setq gc-cons-threshold (* 2 1000 1000))
