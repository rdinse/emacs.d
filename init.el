(require 'package)

;;
;; Packages
;;

;; List the packages you want
(setq package-list '(gruvbox-theme
		     smartparens
		     magit
		     helm
		     flycheck
		     company
		     auctex
		     evil
                     evil-leader
                     evil-mc))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

;; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;
;; Server
;;

(load "server")
(unless (server-running-p) (server-start))

;;
;; Helm
;;

;; Add current dir to helm-mini
(setq helm-mini-default-sources '(helm-source-buffers-list
				  helm-source-recentf
				  helm-source-files-in-current-dir
				  helm-source-buffer-not-found))

;;
;; Evil
;;

(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-mode t)
(setq evil-leader/in-all-states 1)
;; unbind evil dafault SPC of forward char
(define-key evil-normal-state-map (kbd "SPC") nil)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "f" 'helm-find-files
  "b" 'helm-buffers-list
  "m" 'delete-other-windows
  "es" 'eval-last-sexp
  "er" 'eval-and-replace
  "<SPC>" 'helm-mini
  "qr" 'restart-emacs
  "hdk" 'describe-key
  "ji" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "jt" '(lambda () (interactive) (setq default-directory "~/Thesis") (helm-find-files))
  "w" 'save-buffer)

;;; esc always quits
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'keyboard-quit)

;;
;; Text editing
;;

(save-place-mode 1)

(require 'smartparens-config)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'acutex-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)

(require 'flyspell)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;;
;; UI
;;

(set-face-attribute 'default nil :height 110)
(load-theme 'gruvbox-light-hard t)

(tool-bar-mode -1)
(setq inhibit-startup-screen t)

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 100))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 76) ; height of menu bar etc.
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)

;;
;; Misc. functions
;;

(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
							   #'launch-separate-emacs-under-x
							 #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

;;
;; Custom variables
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(package-selected-packages (quote (evil-mc evil-leader evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
