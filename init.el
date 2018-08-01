(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

;; No splash screen
(setq inhibit-startup-message t)

;; Window
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(height . 300))
(add-to-list 'default-frame-alist '(width . 80))

;; Set up package management with `use-package`

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;; Sane defaults ;;;;

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
  :bind (("C-c p ." . helm-projectile-find-file-dwim)))

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

(use-package reason-mode
  :ensure t)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :ensure t)

(use-package white-theme
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8dce5b23232d0a490f16d62112d3abff6babeef86ae3853241a85856f9b0a6e7" "d97baf5a34c87b05508739505cad03438cde8efa2a0d350c7773f2a8bc26a50d" "099c44618d7660548701d4f495a8c23a85103bc7b87fec33c9db4cd099a4adaf" default)))
 '(package-selected-packages
   (quote
    (exec-path helm-ag basic-theme white-theme twilight-bright-theme reason-mode multiple-cursors magit helm-projectile helm projectile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
