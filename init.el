;;
;; Packages
;;

(require 'package)

;; List the packages you want
(setq package-list '(
                     gruvbox-theme    ; High-contrast yellow page theme.
                     smartparens      ; Parentheses auto-completion.
                     magit            ; Git utilities.
                     helm             ; Buffer name and file browsing completion.
                     flycheck         ; Linter and spell-checker.
                     company          ; Auto-completion menu.
                     auctex           ; LaTeX mode.
                     ob-ipython       ; Run Jupyter Notebook in org-mode.
                     restart-emacs    ; Restart Emacs.
                     dash-at-point    ; Open Dash documentation at point (macOS).
                     dtrt-indent      ; Automatically infer indentation style.
                     evil             ; VIM key-bindings for Emacs.
                     evil-surround    ; Utilities for manipulating parentheses.
                     evil-lisp-state  ; Utilities for evaluating LISP code in buffer.
                     evil-leader      ; Set up a leader key for key sequences in NORMAL mode.
                     evil-mc          ; Multiple cursors for evil-mode.
                     ))

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
;; Basic settings
;;

;; OS X Emacs.app settings
(when (and (eq system-type 'darwin) (display-graphic-p))
  ;; (exec-path-from-shell-initialize)
  (let ((path (shell-command-to-string ". ~/.zshenv; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path (append (split-string-and-unquote path ":") exec-path)))
  ;; See customize-group RET ns RET for configuration of modifier keys.
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (global-unset-key (kbd "M-h"))
  ;; (define-key org-mode-map (kbd "M-h") nil) FIXME
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-W") 'maybe-delete-frame-buffer)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 2)))  ;; Set scroll speed
  (setq mouse-wheel-progressive-speed nil)             ;; Don't accelerate scrolling
  (setq scroll-conservatively 6)
  ;; (setq scroll-down-aggressively 0.8)
  ;; (setq scroll-up-aggressively 0.2)
  ;; (setq scroll-step 6)
  (global-set-key (kbd "<wheel-right>") 'maybe-delete-frame-buffer)
  (global-set-key (kbd "M-W") 'maybe-delete-frame-buffer)
  ;; Turn on horizontal scrolling with mouse wheel.
  (global-set-key [wheel-left] 'scroll-right)
  (global-set-key [wheel-right] 'scroll-left)
  )

;; Mouse cursor in terminal mode.
(xterm-mouse-mode 1)

(setq avy-all-windows 'all-frames)

;; Keep focus while navigating help buffers.
(setq help-window-select 't)

;; Start *scratch* in text mode to avoid loading elisp mode on start-up.
(setq initial-major-mode 'text-mode)

;;
;; UI
;;

(set-face-attribute 'default nil :height 110)
(load-theme 'gruvbox-light-hard t)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq frame-title-format '("%b %+ %f"))
(setq-default dired-listing-switches "-alhv")
(setq ring-bell-function 'ignore
      visible-bell nil)
(global-hl-line-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
'(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq my-menu-bar-height (if (eq system-type 'darwin) 44 76))

(defun set-frame-size-according-to-resolution ()
  "Sets the frame size according to the screen resolution."
  (interactive)
  (if window-system
  (progn
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 100))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) my-menu-bar-height)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)

;; Highlighting of special words for selected modes.
;; <http://www.metasyntax.net/unix/dot-emacs.html>
(make-face 'taylor-special-words)
(set-face-attribute 'taylor-special-words nil :foreground "White" :background "Firebrick")
(let ((pattern "\\<\\(FIXME\\|TODO\\|NOTE\\|WARNING\\|BUGS\\|TO DO\\|HACK\\)"))
(mapc
    (lambda (mode)
    (font-lock-add-keywords mode `((,pattern 1 'taylor-special-words prepend))))
    '(ada-mode c-mode emacs-lisp-mode LaTeX-mode latex-mode
	    tex-mode TeX-latex-mode java-mode haskell-mode
	    literate-haskell-mode html-mode org-mode lisp-mode
	    php-mode python-mode ruby-mode scheme-mode sgml-mode
	    sh-mode sml-mode markdown-mode)))

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
(require 'evil-lisp-state)
(require 'evil-mc)
(global-evil-mc-mode 1)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-mode t)
(setq evil-leader/in-all-states 1)
;; unbind evil dafault SPC of forward char
(define-key evil-normal-state-map (kbd "SPC") nil)
(setq-default evil-want-fine-undo t)
(evil-leader/set-leader "<SPC>")
(require 'evil-surround)
;; Make * and # search by symbol instead of by word.
(setq-default evil-symbol-word-search 'symbol)
(evil-select-search-module 'evil-search-module 'evil-search)
(global-evil-surround-mode 1)
;; https://github.com/syl20bnr/spacemacs/blob/master/layers/+distributions/spacemacs-base/keybindings.el
;; https://github.com/syl20bnr/spacemacs/tree/master/layers/+lang
(evil-leader/set-key
  "qr" 'restart-emacs
  "<SPC>" 'helm-mini
  "f" 'helm-find-files
  "b" 'helm-buffers-list
  "m" 'delete-other-windows
  "o" 'other-window
  "r" 'evil-replace-symbol-at-point
  "gn" 'flycheck-next-error
  "gN" 'flycheck-previous-error
  "el" 'lisp-state-eval-sexp-end-of-line
  "er" 'eval-and-replace
  "d" 'dash-at-point
  "hk" 'describe-key
  "hf" 'describe-function
  "l" 'avy-goto-line
  "cn" 'evil-mc-make-and-goto-next-match
  "cs" 'evil-mc-skip-and-goto-next-match
  "ca" 'evil-mc-make-all-cursors
  "cg" 'evil-mc-undo-all-cursors
  "cN" 'evil-mc-make-and-goto-prev-match
  "cb" 'evil-mc-make-cursor-below 
  "ci" 'my-evil-mc-insert-numbers-at-each-cursor
  "cx" 'my-evil-mc-insert-arb-text-at-each-cursor
  "ji" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "jn" '(lambda () (interactive) (find-file "~/Org/notes.org"))
  "jt" '(lambda () (interactive) (setq default-directory "~/Thesis") (helm-find-files))
  "w" 'save-buffer
  )

(defun evil-mc-make-cursor-below ()
  "Add cursor at character below"
  (interactive)
  (evil-mc-make-cursor-here)
  (evil-mc-pause-cursors)
  (next-line)
  (evil-mc-resume-cursors)
  )

(require 'thingatpt)
(defun evil-replace-symbol-at-point ()
  "Opens a replacement prompt containing the symbol at the point."
  (interactive)
  (evil-ex (concat ".,$s/\\(" (regexp-quote (symbol-name (symbol-at-point))) "\\)/")))

;; Use [escape] to quit everything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(global-set-key [double-mouse-1] 'browse-url-at-point)

;; Use S-<left> and S-<right> in normal mode to initiate visual
(define-key evil-normal-state-map (kbd "S-<left>")
  (lambda ()
    (interactive)
    (evil-visual-char)
    (backward-char)))
(define-key evil-normal-state-map (kbd "S-<right>")
  (lambda ()
    (interactive)
    (evil-visual-char)
    (forward-char)))
;; Since visual inherits from normal state, if no collision, we need a reset:
(define-key evil-visual-state-map (kbd "S-<left>")
  #'backward-char)
(define-key evil-visual-state-map (kbd "S-<right>")
  #'forward-char)


;;(define-key evil-normal-state-map
;;		"+" 'spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt)
;;(define-key evil-normal-state-map
;;		"-" 'spacemacs/evil-numbers-transient-state/evil-numbers/dec-at-pt)

(define-key evil-motion-state-map (kbd "<backspace>") 'smex)
;; (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)

;; Helm mini (M-RET to open selected buffer in new frame)
;; (define-key helm-buffer-map (kbd "M-RET") #'helm-buffer-switch-other-frame)

;; smartparens
;;(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
;;(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
;;(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
;;(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;; Take back your Emacs commands
(define-key evil-normal-state-map "\M-s" 'save-buffer)
(define-key evil-insert-state-map "\M-s" 'save-buffer)
(define-key evil-visual-state-map "\M-s" 'save-buffer)
(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-visual-state-map "\C-y" 'yank)
(define-key evil-normal-state-map "\C-e" 'end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-normal-state-map "\C-w" 'evil-delete)
(define-key evil-insert-state-map "\C-w" 'evil-delete)
(define-key evil-visual-state-map "\C-w" 'evil-delete)
(define-key evil-insert-state-map "\C-r" 'search-backward)

;; C-x f to find-file as fill size is rarely used
(global-set-key (kbd "C-x f") 'find-file)

;; C-x (arrow) to move between windows
(global-set-key (kbd "C-S-x <up>")    'windmove-up)
(global-set-key (kbd "C-S-x <down>")  'windmove-down)
(global-set-key (kbd "C-S-x <right>") 'windmove-right)
(global-set-key (kbd "C-S-x <left>")  'windmove-left)

;; Increase/decrease at point
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Disable read-only warnings (https://emacs.stackexchange.com/q/19742/8847)
(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only signal; pass the rest to the default handler."
  (when (not (eq (car data) 'buffer-read-only))
    (command-error-default-function data context caller)))
  
  (setq command-error-function #'my-command-error-function)

;;
;; Text editing & languages
;;

(save-place-mode 1)
(setq create-lockfiles nil)
(setq mark-ring-max 64)
(setq global-mark-ring-max 128)
(setq save-interprogram-paste-before-kill t)
(setq disabled-command-function nil)  ; Disable warnings for scrolling commands.
(setq-default indent-tabs-mode nil)
(setq longlines-show-hard-newlines t)
(setq delete-by-moving-to-trash t)

;; Use only spaces and no tabs.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq-default evil-shift-round nil)
(setq my-fill-column 97)
(setq-default fill-column 'my-fill-column)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Auto-fill
(add-hook 'text-mode 'auto-fill-mode)

;; Indentation style inference
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Make tab key first indent and then complete. not necessary with company
;; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)
(add-hook 'after-init-hook 'global-company-mode)

(require 'smartparens-config)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'LaTeX-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

(global-flycheck-mode)

(require 'flyspell)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-mode)

(add-hook 'js-mode-hook
	  (lambda ()
	    (setq js-indent-level 2)))

(add-hook 'haskell-mode-hook
	  (lambda ()
	    (setq haskell-enable-ghc-mod-support nil)
	    (setq haskell-enable-ghci-ng-support t)
	    (setq haskell-enable-hindent-style "gibiansky")
	    (setq haskell-enable-shm-support nil)))

;;(setq python-python-command "/Users/robind/.pyenv/shims/python3")
(add-hook 'python-mode-hook
	(lambda ()
	    (python-docstring-mode)
	    (setq indent-tabs-mode nil)
	    (setq python-test-runner 'pytest)
	    (setq python-fill-column 'my-fill-column)
	    (setq python-indent 2)))

(setq clojure-enable-fancify-symbols t)

(setq-default shell-default-height 20)
(setq-default shell-default-position 'bottom)
(setq-default shell-enable-smart-eshell t)
(setq-default shell-default-shell 'eshell)

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default c-default-style "linux")
(setq-default c-basic-offset 2)

(add-hook 'c-mode-hook (lambda ()
			(setq fill-column 'my-fill-column)
			;; (setq comment-column 60)
			))

;;
;; Misc. functions
;;

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it is visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))

(defun move-text-internal (arg)
  "Function to move text lines."
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (mark-whole-lines)
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
	(forward-line)
	(when (or (< arg 0) (not (eobp)))
	  (transpose-lines arg)
	  (when (and (eval-when-compile
		       '(and (<= emacs-major-version 24)
			     (>= emacs-minor-version 3)))
		     (< arg 0))
	    (forward-line -1)))
	(if (> arg 0) (forward-line -1)))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
    arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
    arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

;; Bury *scratch* buffer instead of killing it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
	(progn
	  (erase-buffer)
	  (bury-buffer))
      ad-do-it)));

;;
;; Org-mode
;;

;; Load custom theme to make tables and checkboxes monspaced:
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'org-mode-helvetica t)
(add-hook 'org-mode-hook (lambda ()
						   (turn-on-auto-fill)
						   (setq fill-column 120)
						   (setq comment-column 130)
						   ;; Non-monospaced font in org-mode
						   (variable-pitch-mode t)
						   ;; Avoid blank lines between list items
						   (setcdr (assoc 'plain-list-item org-blank-before-new-entry) nil)
						   ;; Keep M-TAB for `completion-at-point'
						   ;;(define-key flyspell-mode-map "\M-\t" nil)
						   ))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode) ;; Requires Ispell
(setq org-bullets-bullet-list '("◯" "◼" "◆" "・"))
(setq org-list-indent-offset 4)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   ;; other languages..
   ))

;;
;; AucTeX
;;

(setq-default TeX-error-overview-open-after-TeX-run t)
(setq-default LaTeX-item-indent 0)
(setq-default LaTeX-indent-level 4)
(setq-default TeX-auto-save t)
(setq-default TeX-parse-self t)
(setq-default TeX-PDF-mode t)
(setq-default TeX-source-correlate-method 'synctex)
(setq-default TeX-source-correlate-method 'synctex)
(setq-default TeX-source-correlate-mode t)
(setq-default TeX-source-correlate-start-server t)
;; (setq-default LaTeX-command "pdflatex -synctex=1")

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
    '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(defadvice TeX-LaTeX-sentinel
    (around mg-TeX-LaTeX-sentinel-open-output activate)
"Open output when there are errors."
;; Run `TeX-LaTeX-sentinel' as usual.
ad-do-it
;; Check for the presence of errors.
(when
    (with-current-buffer TeX-command-buffer
	(plist-get TeX-error-report-switches (intern (TeX-master-file))))
    ;; If there are errors, open the output buffer.
    (TeX-recenter-output-buffer nil)))

;; Auto-raise Emacs on activation
(defun raise-emacs-on-aqua() 
(shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)

;; Bind sync to mouse click
(add-hook 'LaTeX-mode-hook (lambda () (local-set-key (kbd "<S-s-mouse-1>") #'TeX-view)))

;;
;; Custom variables
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil-mc evil-leader evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
