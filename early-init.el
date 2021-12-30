;; -*- lexical-binding: t; -*-

;; Melhora a performance de inicialização
(setq gc-cons-threshold (* 50 1000 1000))

;; Visual mínimo
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(tooltip-mode      -1)
(set-fringe-mode   10)
(blink-cursor-mode  0)

;; Fonte
(defun ed-set-font ()
  (set-frame-font "Source Code Pro-12" nil t))

(add-hook 'server-after-make-frame-hook #'ed-set-font)
