;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-startup-screen t)
(setq-default inhibit-message t
              initial-scratch-message nil
              initial-major-mode 'fundamental-mode)

(defun ed-set-font ()
  "Carrega a minha fonte de preferĂȘncia"
  (set-frame-font "Iosevka Slab-16" nil t))

(add-hook 'server-after-make-frame-hook #'ed-set-font)

(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(tooltip-mode      -1)
(set-fringe-mode   10)
(blink-cursor-mode  0)
