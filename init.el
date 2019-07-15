(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

;; ----- Install packages ----- ;;

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defun install-packages (packages)
  (let ((refreshed nil))
    (dolist (pack packages)
      (unless (package-installed-p pack)
        (unless refreshed
          (package-refresh-contents)
          (setq refreshed t))
        (package-install pack)))))

(install-packages '(magit
                    markdown-mode
                    smex
                    use-package))

;; ----- keybind ----- ;;

(mapc '(lambda (pair)
         (global-set-key (kbd (car pair)) (cdr pair)))
      '(("M-g"  . goto-line)
        ("C-h"  . delete-backward-char)
        ("C-z"  . nil)
        ("C-_"  . undo)
        ("C-\\" . undo)
        ("C-o"  . nil)
        ("M-*"  . pop-tag-mark)
        ("C-x ;" . comment-region)
        ("C-x :" . uncomment-region)
        ("C-x C-i"   . indent-region)))

;; ----- Environment ----- ;;

(setq scroll-conservatively 1)
(set-face-foreground 'font-lock-comment-face "#ee0909")
(show-paren-mode t)

(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t))

(use-package smex
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

;; -- -- ;;

;; mode-line
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))
(which-function-mode 1)

;; backup
(setq delete-auto-save-files t)
(setq backup-inhibited t)

;; use space instead of tab
(setq-default indent-tabs-mode nil)

;; dired
(defvar my-dired-before-buffer nil)
(defadvice dired-up-directory
    (before kill-up-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-up-directory
    (after kill-up-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
      (kill-buffer my-dired-before-buffer)))

(setq dired-listing-switches "-lXa")

;; ----- go settings ----- ;;

(install-packages '(go-mode
                    eglot
                    flycheck
                    company
                    exec-path-from-shell))

;; - company - ;;

(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 1)
  (setq company-minimum-prefix-length 1)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
    (setq company-selection-wrap-around t))

;; - eglot - ;;

(use-package eglot
  :bind
  (("M-." . xref-find-definitions)
   ("M-[" . xref-find-references))
  :config
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  :init
  (add-hook 'js-mode-hook 'eglot-ensure)
    (add-hook 'go-mode-hook 'eglot-ensure))

;; - go - ;;

(let ((envs '("GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

(defun my-gofmt-hook ()
  (unwind-protect
      (progn
        (remove-hook 'before-save-hook 'my-gofmt-hook)
        (save-buffer)
        (shell-command (concat "goimports -w " (buffer-file-name)))
        (revert-buffer t t t))
    (add-hook 'before-save-hook 'my-gofmt-hook)))

;; FIXME:
;; (setq gofmt-command "goimports")
;; (add-hook 'before-save-hook 'gofmt-before-save)
;;
;; The above settings on ":config" is an ordinal solution.
;; However, it got into the following error.
;;   "Invalid rcs patch or internal error in go--apply-rcs-patch"
;; So I created an original hook.

(use-package go-mode
  :commands go-mode
  :defer t
  :config
  (add-hook 'before-save-hook 'my-gofmt-hook)
  ;; The following is required to avoid wrong analysis by LSP server.
  (add-hook 'after-save-hook
            '(lambda ()
               (revert-buffer t t t))))

;; ----- Other libraries ----- ;;

;; display the directory name of the file when files that have a same name are opened
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; --------------------- ;;
;; --- auto settings --- ;;
;; --------------------- ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-merge-arguments (quote ("--no-ff")))
 '(package-selected-packages
   (quote
    (flymake jsonrpc wgrep smex w3m paredit markdown-mode magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
