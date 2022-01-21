;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 50 1000 1000))

(setq inhibit-startup-screen t)
(setq-default initial-scratch-message nil)
(setq inhibit-startup-echo-area-message "eduardo")

(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(tooltip-mode      -1)
(set-fringe-mode   10)
(blink-cursor-mode  0)
