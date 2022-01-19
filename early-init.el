;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 50 1000 1000))

(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(tooltip-mode      -1)
(set-fringe-mode   10)
(blink-cursor-mode  0)

(defun ed-set-font ()
  (set-frame-font "Inconsolata-14" nil t))

(add-hook 'server-after-make-frame-hook #'ed-set-font)
