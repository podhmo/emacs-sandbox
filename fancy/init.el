;;; -*- coding: utf-8; lexical-binding: t -*-
;;; init.el -- fancy: make emacs on WSLg look as good as possible
;;;
;;; launch: emacs --init-dir=<this dir>

(defvar fancy:dir (file-name-directory (or load-file-name default-directory))
  "Directory that contains this init file.")

(defvar fancy:nix-p (getenv "FANCY_NIX")
  "Non-nil when launched via the nix wrapper: elisp deps come from the
nix store and fancy:dir is read-only, so writable state goes elsewhere.")

(defvar fancy:data-dir
  (if fancy:nix-p
      (expand-file-name "emacs-fancy"
                        (or (getenv "XDG_DATA_HOME") "~/.local/share"))
    fancy:dir)
  "Writable directory for state files (custom.el, elpa when non-nix).")

(setq custom-file (expand-file-name "custom.el" fancy:data-dir))

;;; packages -----------------------------------------------------------
(require 'package)
(setq package-user-dir (expand-file-name "elpa" fancy:data-dir))
(unless fancy:nix-p
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(package-initialize)
(unless (or fancy:nix-p package-archive-contents)
  (package-refresh-contents))

;; install everything up front so :ensure never has to guess
(defvar fancy:packages
  '(doom-themes
    solaire-mode
    nerd-icons
    doom-modeline
    spacious-padding
    vertico
    marginalia
    orderless
    nerd-icons-completion
    posframe
    vertico-posframe
    hl-todo
    rainbow-delimiters
    markdown-mode
    mixed-pitch
    nerd-icons-dired
    which-key-posframe
    ligature
    diff-hl
    page-break-lines
    minions
    indent-bars
    corfu
    kind-icon
    dashboard))

;; under nix every dep is already on load-path via emacsWithPackages
(unless fancy:nix-p
  (dolist (pkg fancy:packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

;;; basics -------------------------------------------------------------
(use-package emacs
  :custom
  (use-short-answers t)
  (ring-bell-function #'ignore)
  (echo-keystrokes 0.1)
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)
  (scroll-conservatively 101)
  (scroll-margin 3)
  (x-underline-at-descent-line t)
  :init
  (setq-default line-spacing 0.12
                fill-column 100
                indent-tabs-mode nil
                cursor-type 'bar
                cursor-in-non-selected-windows nil)
  (global-auto-revert-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (pixel-scroll-precision-mode 1)
  (blink-cursor-mode -1)
  ;; line numbers only in file buffers: the number gutter keeps the frame's
  ;; unremapped default bg, which would leave a light strip on solaire-dimmed
  ;; "unreal" buffers (scratch, help, posframe-adjacent buffers)
  (add-hook 'prog-mode-hook
            (lambda () (when buffer-file-name (display-line-numbers-mode 1))))
  (add-hook 'dired-mode-hook #'hl-line-mode))

;;; font -----------------------------------------------------------------
;; default: JetBrains Mono (downloaded to ~/.local/share/fonts)
;; CJK:     Noto Sans CJK JP (system)
;; symbols: Symbols Nerd Font Mono (~/.local/share/fonts), Noto Color Emoji
(defun fancy:setup-fonts ()
  (when (display-graphic-p)
    (when (member "JetBrains Mono" (font-family-list))
      (set-face-attribute 'default nil
                          :family "JetBrains Mono"
                          :height 130
                          :weight 'regular)
      (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 1.0))
    (when (member "Ubuntu Sans" (font-family-list))
      (set-face-attribute 'variable-pitch nil :family "Ubuntu Sans" :height 1.05))
    ;; CJK -- keep punctuation/full-width glyphs consistent with han/kana
    (when (member "Noto Sans CJK JP" (font-family-list))
      (dolist (cs '(han kana cjk-misc))
        (set-fontset-font t cs "Noto Sans CJK JP" nil 'prepend))
      (set-fontset-font t '(#x3000 . #x303f) "Noto Sans CJK JP" nil 'prepend))
    ;; symbols -> nerd font, emoji -> color emoji
    (when (member "Symbols Nerd Font Mono" (font-family-list))
      (set-fontset-font t 'symbol "Symbols Nerd Font Mono" nil 'prepend))
    (when (member "Noto Color Emoji" (font-family-list))
      (set-fontset-font t 'emoji "Noto Color Emoji" nil 'prepend))))

(add-hook 'after-init-hook #'fancy:setup-fonts)

;;; theme ----------------------------------------------------------------
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  ;; internal border (outer padding) should match the frame background
  (set-face-background 'internal-border (face-background 'default))
  ;; line numbers keep their own bg under solaire's minibuffer dimming --
  ;; clear it so the whole frame dims uniformly
  (set-face-background 'line-number 'unspecified)
  (set-face-background 'line-number-current-line 'unspecified))

;; slightly dimmer background for "unreal" buffers (doom trick)
(use-package solaire-mode
  :config (solaire-global-mode 1))

;;; mode line --------------------------------------------------------------
(use-package nerd-icons
  :defer t) ; requires "Symbols Nerd Font Mono" in ~/.local/share/fonts

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 32)
  (doom-modeline-bar-width 4)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil))

;;; padding / breathing room -----------------------------------------------
(use-package spacious-padding
  :init (spacious-padding-mode 1)
  :custom
  (spacious-padding-subtle-mode-line nil)
  (spacious-padding-widths
   '(:internal-border-width 20
     :header-line-width 4
     :mode-line-width 6
     :custom-button-width 3
     :tab-width 4
     :right-divider-width 24
     :scroll-bar-width 8
     :fringe-width 12)))

;; thin separator between windows instead of default reverse-video divider
(setq window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode 1)

;;; minibuffer --------------------------------------------------------------
(use-package vertico
  :init (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-count 14))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package nerd-icons-completion
  :after marginalia
  :config (nerd-icons-completion-mode 1))

;; spotlight-like centered minibuffer popup
(use-package vertico-posframe
  :after vertico
  :init (vertico-posframe-mode 1)
  :custom
  (vertico-posframe-width 110)
  (vertico-posframe-border-width 2)
  (vertico-posframe-parameters
   '((left-fringe . 14)
     (right-fringe . 14)
     (internal-border-width . 16))))

;;; syntax highlighting garnish ----------------------------------------------
(use-package hl-todo
  :init (global-hl-todo-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; horizontal line for ^L instead of the literal formfeed glyph
(use-package page-break-lines
  :init (global-page-break-lines-mode 1))

;;; which-key ----------------------------------------------------------------
;; which-key is built into emacs 30; the melpa package only adds posframe
(use-package which-key
  :init (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.4)
  (which-key-sort-order 'which-key-key-order-alpha))

(use-package which-key-posframe
  :after which-key
  :init (which-key-posframe-mode 1)
  :custom
  (which-key-posframe-poshandler #'posframe-poshandler-frame-center))

;;; ligatures -----------------------------------------------------------------
;; JetBrains Mono ships these glyphs; harmless on fonts that lack them
(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode
   '("==" "!=" "!==" "===" "==>" "=>>" "=<<" "=/=" "->>" "->"
     "-->" "<--" "<->" "<-<" "<<-" "<==" "<=>" "=>" ">=" ">>="
     ">->" "<~>" "<~~" "~~>" "~>" "~=" "/=" "/==" "//" "///"
     "/*" "*/" "**" "***" "+++" "&&&" "|||" "||=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "?=" "?." "??" ";;" "__" "~~"
     "(*" "*)" "www" "%%" ".=" ".." ".-"))
  (global-ligature-mode 1))

;;; modeline declutter ----------------------------------------------------------
;; fold minor-mode lighters into a menu; doom-modeline must opt in to show it
(use-package minions
  :init (minions-mode 1)
  :custom
  (doom-modeline-minor-modes t))

;;; VCS diff in fringe ----------------------------------------------------------
(use-package diff-hl
  :init (global-diff-hl-mode 1)
  :hook (dired-mode . diff-hl-dired-mode))

;;; indent guides ---------------------------------------------------------------
(use-package indent-bars
  :hook (prog-mode . indent-bars-mode))

;;; in-buffer completion --------------------------------------------------------
(use-package corfu
  :init (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  ;; svg-lib caches rendered icons under user-emacs-directory by default,
  ;; which is read-only in the nix store -- keep it under fancy:data-dir
  (svg-lib-icons-dir (expand-file-name ".cache/svg-lib/" fancy:data-dir))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; dashboard -------------------------------------------------------------------
(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents . 5) (projects . 5)))
  (dashboard-center-content t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  :config
  (recentf-mode 1)
  (dashboard-setup-startup-hook))

;;; markdown -------------------------------------------------------------------
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-hide-markup t)
  (markdown-list-item-bullets '("-")))

;; prose in variable-pitch, code/tables stay monospace -- bear/typora feel
(use-package mixed-pitch
  :hook (markdown-mode . mixed-pitch-mode))

;;; misc ------------------------------------------------------------------------
;; shell colors
(add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

;; dired icons
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :disabled t)

(load custom-file t)
