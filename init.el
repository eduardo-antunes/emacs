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

(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)  ; default is 15s
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024))) ;; 16MB

(set-language-environment "UTF-8")
(setq default-input-method nil)

(setq delete-by-moving-to-trash t)
(setq default-input-method nil)

(setq user-full-name       "Eduardo Antunes"
      user-mail-address    "eduardoantunes986@gmail.com")

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq-default custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defalias #'yes-or-no-p #'y-or-n-p)

(defun ed-make-directory-if-non-existing ()
  (let ((parent-dir (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-dir))
               (yes-or-no-p "This file's directory doesn't exist. Create it? ")
      (make-directory parent-dir t)))))

(add-to-list 'find-file-not-found-functions #'ed-make-directory-if-non-existing)

(add-hook 'before-save-hook #'whitespace-cleanup)

(use-package which-key
  :defer 0
  :custom
  (which-key-sort-order #'which-key-prefix-then-key-order)
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(setq inhibit-startup-echo-area-message "eduardo")

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :after undo-tree
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-undo-system 'undo-tree)
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
    "c"   '(:ignore t :which-key "compilation")
    "cc"  #'compile
    "cr"  #'recompile

    ;; buffer operations
    "b"  '(switch-to-buffer :which-key "buffers")
    "k"  #'kill-buffer

    ;; keymaps
    "w"   '(:keymap evil-window-map :which-key "window")
    "h"   '(:keymap help-map :which-key "help")
    "o"   '(:ignore t :which-key "open")
    "m"   '(:ignore t :which-key "mode")))

(ed-set-font)
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package minions
  :custom
  (minions-mode-line-lighter "...")
  (minions-prominent-modes '(flyspell-mode text-scale-mode))
  :config (minions-mode 1))

(setq display-time-format "%H:%M"
      display-time-default-load-average nil
      display-time-interval 60)
(display-time-mode 1)

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

(use-package smooth-scrolling
  :init (smooth-scrolling-mode 1))

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
  (org-indent-mode 1)
  (visual-line-mode 1)
  (dolist (pair '(("#+begin_src" . ???)
                  ("#+BEGIN_SRC" . ???)
                  ("#+end_src"   . ???)
                  ("#+END_SRC"   . ???)))
    (add-to-list 'prettify-symbols-alist pair))
  (prettify-symbols-mode))

(use-package org
  :defer t
  :hook (org-mode . ed-org-mode-setup)
  :custom
  (org-hide-emphasis-markers t))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("??")))

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

(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'ed-org-babel-tangle-config)))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
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
     " ?? ")))

(defun ed-eshell-setup ()
  (require 'evil-collection-eshell)
  (evil-collection-eshell-setup)
  ;; Salve comandos no hist??rico ?? medida que eles forem inseridos
  (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)
  ;; Reduza o buffer do eshell quando ele exceder o m??ximo de linhas
  (add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer))

(use-package eshell
  :ensure nil
  :hook (eshell-first-time-mode . ed-eshell-setup)
  :custom
  (eshell-banner-message "GNU emacs shell for fun and profit\n\n")
  (eshell-history-size 10000)
  (eshell-hist-ignore-dups t)
  (eshell-buffer-maximum-lines 10000)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-prompt-regexp "^[^??#]*[??#] ")
  (eshell-prompt-function #'ed-eshell-prompt)
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
  :commands
  (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  :general
  (ed-leader-key
    "g" '(magit-status :which-key "git")))

(ed-leader-key 'smerge-mode-map
  "s" '(:keymap smerge-basic-map :which-key "smerge"))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package sly
  :hook (lisp-mode . sly))

(add-hook 'prog-mode-hook   #'electric-pair-mode)
(add-hook 'eshell-mode-hook #'electric-pair-local-mode)

(setq-default inhibit-message nil)
