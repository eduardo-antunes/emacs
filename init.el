;; -*- lexical-binding: t; -*-
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

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

(use-package smooth-scrolling
  :init (smooth-scrolling-mode 1))

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

  (general-create-definer ed-leader-key
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-c")

  (ed-leader-key
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
  :custom
  (which-key-sort-order #'which-key-prefix-then-key-order)
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
                eww-mode-hook
                calendar-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(ed-set-font)

(use-package modus-themes
  :custom
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-mode-line '(borderless 4))
  :general
  (ed-leader-key
    "t" #'modus-themes-toggle))

(let ((time (string-to-number (format-time-string "%H"))))
  (if (and (> time 5) (< time 18))
      (modus-themes-load-operandi)
    (modus-themes-load-vivendi)))

(use-package minions
  :custom
  (minions-mode-line-lighter "...")
  (minions-prominent-modes '(flyspell-mode text-scale-mode))
  :config (minions-mode 1))

(setq display-time-format "%H:%M"
      display-time-default-load-average nil
      display-time-interval 60)

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(defun ed-minibuffer-backward-kill (arg)
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
    "<backspace>"  #'ed-minibuffer-backward-kill)

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

(defun ed-org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (dolist (pair '(("#+begin_src" . ?λ)
                  ("#+BEGIN_SRC" . ?λ)
                  ("#+end_src"   . ?λ)
                  ("#+END_SRC"   . ?λ)))
    (add-to-list 'prettify-symbols-alist pair))
  (prettify-symbols-mode))

(use-package org
  :defer t
  :hook (org-mode . ed-org-mode-setup)
  :custom
  (org-hide-emphasis-markers t)
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "○")))

(defun ed-org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ed-org-mode-visual-fill))

(defun ed-org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ed-org-babel-tangle-config)))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-Al --group-directories-first")
  :general
  (general-def 'normal 'dired-mode-map
    "SPC" nil
    "h"   #'dired-up-directory
    "l"   #'dired-find-file))

(defun ed-eshell-prompt ()
  (concat
   (eshell/pwd)
   (if (= (user-uid) 0) " # "
     " λ ")))

(defun ed-eshell-setup ()
  (require 'evil-collection-eshell)
  (evil-collection-eshell-setup)

  ;; Salve comandos no histórico à medida que eles forem inseridos
  (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)

  ;; Reduza o buffer do eshell quando ele exceder o máximo de linhas
  (add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer)

  (setq eshell-history-size 10000
        eshell-hist-ignore-dups t
        eshell-buffer-maximum-lines 10000
        eshell-prompt-function #'ed-eshell-prompt
        eshell-prompt-regexp "^[^λ#]*[λ#] "
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :ensure nil
  :hook (eshell-first-time-mode . ed-eshell-setup)
  :general
  (ed-leader-key
    "oe" #'eshell))

(use-package eshell-syntax-highlighting
  :after eshell
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package vterm
  :general
  (ed-leader-key
    "ot" '(vterm-other-window :which-key "terminal")
    "oT" '(vterm :which-key "terminal full")))

(use-package pomm
  :commands (pomm pomm-start)
  :custom
  (pomm-state-file-location
   (no-littering-expand-var-file-name "pomm.el"))
  :general
  (ed-leader-key
    "op" '(pomm :which-key "pomodoro")))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :general
  (ed-leader-key
    "g" '(magit-status :which-key "git")))

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  (org-mode . yas-minor-mode))

(use-package yasnippet-snippets)

(use-package projectile
  :init
  (setq projectile-keympa-prefix nil)
  :config
  (projectile-mode)
  :general
  (ed-leader-key
    "p" '(:keymap projectile-command-map :which-key "project")))

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
  (ed-leader-key
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

(defun ed-c-cpp-setup ()
  (c-set-style "cc-mode")
  (lsp-deferred))

(use-package cc-mode
  :hook ((c-mode . ed-c-cpp-setup)
         (c++-mode . ed-c-cpp-setup)))

(use-package nasm-mode
  :mode "\\.asm\\'")

(defun ed-python-setup ()
  (require 'lsp-pyright)
  (lsp-deferred))

(use-package python-mode)
(use-package lsp-pyright
  :hook (python-mode . ed-python-setup))

(use-package dart-mode)

(use-package lsp-dart
  :hook (dart-mode . lsp-deferred)
  :init
  (setq
   lsp-dart-flutter-sdk-dir "~/Downloads/flutter"
   lsp-dart-sdk-dir (concat lsp-dart-flutter-sdk-dir "/bin/cache/dart-sdk")))

(use-package flutter
  :after dart-mode
  :general
  (ed-leader-key dart-mode-map
    "mr" '(flutter-run-or-hot-reload :which-key "hot reload")))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'prog-mode-hook (lambda () (electric-pair-local-mode 1)))
(add-hook 'eshell-mode-hook (lambda () (electric-pair-local-mode 1)))

(setq gc-cons-threshold (* 2 1000 1000))
