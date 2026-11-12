;;; -*- coding: utf-8; lexical-binding: t -*-
;;; early-init.el -- fancy: suppress chrome before the first frame

;; when launched from the nix wrapper the init dir lives in the read-only
;; store; redirect the eln cache somewhere writable before native-comp runs
(when (getenv "FANCY_NIX")
  (startup-redirect-eln-cache
   (expand-file-name "emacs-fancy/eln-cache"
                     (or (getenv "XDG_CACHE_HOME") "~/.cache"))))

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; startup performance
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8
      inhibit-compacting-font-caches t)

;; chrome: no menu bar, no tool bar, no scroll bars
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)

;; frame geometry + outer padding
(push '(width . 130) default-frame-alist)
(push '(height . 36) default-frame-alist)
(push '(internal-border-width . 20) default-frame-alist)

;; avoid flicker on resize
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; don't let native-comp warnings steal a window during startup
(setq native-comp-async-report-warnings-errors nil)
