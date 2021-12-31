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

(setq user-full-name       "Eduardo Antunes"
      user-real-login-name "Eduardo"
      user-login-name      "eduardo"
      user-mail-address    "eduardoantunes986@gmail.com")

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq-default custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defalias #'yes-or-no-p #'y-or-n-p)

(add-hook 'before-save-hook #'whitespace-cleanup)

(setq delete-by-moving-to-trash t)

(setq inhibit-startup-screen t)
(setq-default initial-scratch-message nil)

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

(use-package general
  :after evil
  :config

  (general-create-definer ed/leader-key
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-c")

  (ed/leader-key
    "SPC" #'find-file
    "."   #'dired-jump
    ":"   '(execute-extended-command :which-key "M-x")

    ;; compile operations
    "c"   '(:ignore t :which-key "compile")
    "cc"  #'compile
    "cr"  #'recompile

    ;; buffer operations
    "b"   '(:ignore t :which-key "buffer")
    "bb"  #'switch-to-buffer
    "bd"  #'kill-current-buffer
    "bk"  #'kill-buffer

    ;; keymaps
    "w"   '(:keymap evil-window-map :which-key "window")
    "h"   '(:keymap help-map :which-key "help")
    "o"   '(:ignore t :which-key "open")
    "m"   '(:ignore t :which-key "mode")))

(use-package which-key
  :defer 0
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

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

(use-package doom-themes
  :config
  (doom-themes-org-config))

(use-package modus-themes
  :general
  (ed/leader-key
    "t" #'modus-themes-toggle))

(load-theme 'doom-one t)

(use-package all-the-icons)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (size-indication-mode 1)
  :custom
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(defun ed/minibuffer-backward-kill (arg)
  "Um delete mais conveniente no minibuffer"
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-delete-char arg)))

(use-package vertico
  :general
  (general-def vertico-map
    "C-j"  #'vertico-next
    "C-k"  #'vertico-previous
    "C-l"  #'vertico-exit-input)

  (general-def minibuffer-local-map
    "M-h"          #'backward-kill-word
    "<backspace>"  #'ed/minibuffer-backward-kill)

  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package corfu
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-preselect-first nil)
  :config
  (setq tab-always-indent 'complete)
  (corfu-global-mode 1)
  :general
  (general-def corfu-map
    "TAB"      #'corfu-next
    [tab]      #'corfu-next
    "S-TAB"    #'corfu-previous
    [backtab]  #'corfu-previous))

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

(defun ed/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ed/org-babel-tangle-config)))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-al --group-directories-first")
  :general
  (general-def 'normal 'dired-mode-map
    "h" #'dired-up-directory
    "l" #'dired-find-file))

(defun ed/eshell-prompt ()
  (concat
   (eshell/pwd)
   (if (= (user-uid) 0) " # "
     " λ ")))

(defun ed/configure-eshell ()
  (require 'evil-collection-eshell)
  (evil-collection-eshell-setup)

  ;; Salve comandos no histórico à medida que eles forem inseridos
  (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)

  ;; Reduza o buffer do eshell quando ele exceder o máximo de linhas
  (add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer)

  (setq eshell-history-size 10000
        eshell-hist-ignore-dups t
        eshell-buffer-maximum-lines 10000
        eshell-prompt-function #'ed/eshell-prompt
        eshell-prompt-regexp "^[^λ#]*[λ#] "
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :ensure nil
  :hook (eshell-first-time-mode . ed/configure-eshell)
  :general
  (ed/leader-key
    "oe" #'eshell))

(use-package eshell-syntax-highlighting
  :after eshell
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package vterm
  :general
  (ed/leader-key
    "ot" '(vterm-other-window :which-key "terminal")
    "oT" '(vterm :which-key "terminal+")))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :general
  (ed/leader-key
    "g" '(magit-status :which-key "git")))

(use-package magit-todos ;; mostra TODOs para os arquivos em um repo
  :after magit)

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  (org-mode . yas-minor-mode))

(use-package yasnippet-snippets)

(use-package projectile
  :config (projectile-mode)
  :general
  (ed/leader-key
    "p"  '(:keymap projectile-command-map :which-key "project")))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix nil)
  :custom
  (lsp-enable-snippet t)
  (lsp-completion-provider :none)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-which-key-integration)
  :general
  (ed/leader-key
    "l" '(:keymap lsp-command-map :which-key "lsp")))

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
  (ed/leader-key dart-mode-map
    "mr" '(flutter-run-or-hot-reload :which-key "hot reload")))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  ;; Emprestado do DOOM emacs
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))

(add-hook 'prog-mode-hook (lambda () (electric-pair-local-mode 1)))

(setq gc-cons-threshold (* 2 1000 1000))
