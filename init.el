(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(package-initialize)

;; Bootstrap `use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; No splash screen
(setq inhibit-startup-message t)

;; Window
(defvar default-window-height 40)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(height . default-window-height))
(add-to-list 'default-frame-alist '(width . 80))
(set-face-attribute 'default nil :height 120)

;;;; Sane defaults ;;;;

;; Move custom set variables into different file

(setq custom-file "./custom.el")

;; Allow pasting from system clipboard
(setq x-select-enable-clipboard t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Tabs should be two spaces
(setq-default tab-width 2)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Beginning of line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
		'smarter-move-beginning-of-line)

;; UTF-8

(prefer-coding-system 'utf-8)

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;;; Backups ;;;;

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)

;; Turn off bell

(setq ring-bell-function 'ignore)

;; Automatically reload files that have changed

(global-auto-revert-mode t)

;;;; Package stuff ;;;;

(use-package helm
  :config
  (helm-mode 1)
  (require 'helm-config)

  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-c h" . helm-mini)
         ("C-x C-b" . helm-buffers-list))
  :ensure t)

(use-package helm-ls-git
  :bind (("C-c g" . helm-ls-git-ls)
         ("C-x C-d" . helm-browse-project)
         ("M-y" . helm-show-kill-ring))
  :ensure t)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  :ensure t)

(use-package ag
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package helm-ag
  :config
  (add-to-list 'exec-path "/usr/local/bin/")
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  :config
  (projectile-global-mode t)
  (helm-projectile-on)
  :bind (("C-c p ." . helm-projectile-find-file-dwim)
         ("C-c p p" . helm-projectile-switch-project)
         ("C-c p i" . projectile-invalidate-cache)
         ("C-c p s s" . projectile-ag)))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :ensure t)

;;; Other stuff ;;;

(use-package twilight-bright-theme
  :config
  (load-theme 'twilight-bright t)
  :ensure t)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :ensure t)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :ensure t)

;; Org mode

(use-package org
  :config
  (global-set-key "\C-ca" 'org-agenda)

  (setq org-time-clocksum-use-fractional 1)

  (setq org-agenda-files '("~/org"))

  (add-hook 'org-mode-hook 'visual-line-mode)

  (setq org-duration-format (quote h:mm))

  (define-key global-map "\C-cj" 'org-clock-jump-to-current-clock)

  (setq org-tags-column 0)

  (setq org-default-notes-file (concat org-directory "/notes.org"))

  (define-key global-map "\C-cc" 'org-capture)

  (setq org-capture-templates
        '(("c" "Cultureamp task" entry (file+headline "~/org/notes.org" "Tasks")
           "** TODO %? :cultureamp:\n %i" :prepend t)))

(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-duration-format (quote h:mm))

;; Markdown

(use-package markdown-mode
  :ensure t)

;; Custom functions


(defun frame-single ()
  (interactive)
  (if (window-system)
      (set-frame-size (selected-frame) 80 default-window-height)))

(defun frame-double ()
  (interactive)
  (if (window-system)
      (set-frame-size (selected-frame) 163 default-window-height)))

(defun font-large ()
  (interactive)
  (set-face-attribute 'default nil :height 150))

(defun font-medium ()
  (interactive)
  (set-face-attribute 'default nil :height 120))
