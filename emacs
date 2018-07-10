;; -*-Emacs-Lisp-*-

;; This file is designed to be re-evaled; use the variable first-time
;; to avoid any problems with this.
(defvar first-time t
  "Flag signifying this is the first time that .emacs has been evaled")

;; UTF-8 as default encoding
(set-language-environment 'UTF-8)
(set-default-coding-systems 'utf-8)

;;
;; repositories
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/"))))
  (add-to-list 'package-archives '("marmalade" . (concat proto "://marmalade-repo.org/packages/")) t))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;
;; setup ergonomic keybindings
(use-package xah-fly-keys
  :ensure t
  :init
  ;; no control binding will be touched by xah-fly-keys
  (setq xah-fly-use-control-key nil) ; must come before loading xah-fly-keys
  (setq xah-fly-use-meta-key nil) ; must come before loading xah-fly-keys

  :config
  (xah-fly-keys-set-layout "qwerty") ; required if you use qwerty

  (xah-fly-keys 1)

  ;; Make Escape Key Do C-g
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")))


;;
;; package help to learn new keybindings
;; it displays keybindings in a popup window
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;;
;; enable ido completion everywhere
(use-package ido
  :ensure t
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))


;;
;; allows to use ido for completion of commands in M-x,
;; with enhancements like putting your most-used commands
;; at the front of the list
(use-package smex
  :ensure t

  :config
  ;; Can be omitted. This might cause a (minimal) delay
  ;; when Smex is auto-initialized on its first run.
  (smex-initialize))


;;
;; package to work seamlessly with X
;; clipboard buffer
(use-package xclip
  :ensure t
  :config
  (xclip-mode +1)
  (setq
   x-select-enable-clipboard t
   x-select-enable-primary t
   x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
   x-stretch-cursor t))


;;
;; auto completion
(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))


;;
;; git support
(use-package magit
  :ensure t)

;;
;; similar to magit packages for mercurial
(use-package monky
  :ensure t)

;;
;; manage and navigate projects easily
(use-package projectile
  :ensure t
  :init
  (projectile-mode 1))

;;
;; minor mode for editing parentheses
(use-package paredit
  :ensure t)

;;
;; quickly find any file in a given project
(use-package find-file-in-project
  :ensure t)

;;
;; highlight all occurences in the buffer of
;; the word under the point
(use-package idle-highlight-mode
  :ensure t)

;;
;; package to recevie PATH and some other env variables
;; from shell, so compile and shell-command work like expected
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package solarized-theme
  :ensure t)



;;
;; Keybindings
(global-set-key [f6] 'next-error)
(global-set-key [S-f6] 'previous-error)
(global-set-key [f5] 'recompile)
(global-set-key [S-f5] 'compile)




;; Mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

;;
;; backups
(setq backup-by-copying t)		; make backups by copying
(setq vc-make-backup-files t)		; make backup files even for files
					; covered by version control
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 4
  version-control t)

;; path for backup files
(setq emacs-backup-directory (concat user-emacs-directory "backup"))

;; autocreate backup directory
(if (not (file-exists-p emacs-backup-directory))
    (make-directory emacs-backup-directory t))

;; delete backup files older than a 30 dyas on start
(message "Deleting old backup files...")
(let ((days (* 60 60 24 30))
      (current (float-time (current-time))))
  (dolist (file (directory-files emacs-backup-directory t))
    (when (and (backup-file-name-p file)
	       (> (- current (float-time (nth 5 (file-attributes file))))
		  days))
      (message "%s" file)
      (delete-file file))))


(setq backup-directory-alist
      `((".*" . ,emacs-backup-directory)))

(setq backup-by-copying-when-mismatch t)


;;
;; autosave

;; path for autosave files
(setq emacs-autosave-directory (concat user-emacs-directory "autosave"))

;; autocreate directory for autosave
(if (not (file-exists-p emacs-autosave-directory))
    (make-directory emacs-autosave-directory t))

;; delete autosave files older than a 30 days on start
(message "Deleting old autosave files...")
(let ((days (* 60 60 24 30))
      (current (float-time (current-time))))
  (dolist (file (directory-files emacs-autosave-directory t))
    (when (and (backup-file-name-p file)
	       (> (- current (float-time (nth 5 (file-attributes file))))
		  days))
      (message "%s" file)
      (delete-file file))))

(setq auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))


;;
;; automaticaly save the location of the point when you kill a buffer and
;; returns to it next time you visit the associated file.
(if (< emacs-major-version 25)
    (progn
      (require 'saveplace)
      (setq-default save-place t)
      (setq save-place-file (concat user-emacs-directory "places")))
  (save-place-mode))



;; autodetection of codepage
(when (load "auto-enca" 'noerror)
  (modify-coding-system-alist 'file "" 'enca-detect-coding))


;; command history
(setq savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode t)


;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)


;; checkpatch for C source files
(defun checkpatch()
  (interactive)
  (compile (concat "checkpatch.pl --emacs --terse --file " (buffer-file-name))))

;; Mouse
(global-set-key [mouse-3] 'imenu)


;; Treat 'y' or <CR> as yes, 'n' as no.
(defalias 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Load packages
(require 'desktop)
(setq desktop-save t)			; always save and never ask
(setq desktop-load-locked-desktop t)	; load even locked desktop, in case emacs crashed
(desktop-save-mode 1)

(require 'tar-mode)
(require 'dired)

;; for isearch-forward, make these equivalent: space newline tab hyphen underscore
(setq search-whitespace-regexp "[-_ \t\n]+")


;;
;; C++ mode
(defun my-c++-mode-hook-common ()
  (setq tab-width c-basic-offset)
  (setq c++-auto-hungry-initial-state 'none)
  (setq c++-delete-function 'delete-char)
  (setq c++-tab-always-indent t)
  (setq c-indent-level tab-width)

  (setq c-continued-statement-offset tab-width)
  (setq c++-empty-arglist-indent tab-width)
  (c-set-offset 'case-label '+)		; indent case lagels by
					; c-indent-level, too
  ;; (whitespace-mode)
)

(defun my-c++-mode-hook-tabs ()
  (my-c++-mode-hook-common)
  (setq indent-tabs-mode t)
)

(defun my-c++-mode-hook-spaces ()
  (my-c++-mode-hook-common)
  (setq indent-tabs-mode nil)
)

(add-hook 'c++-mode-hook 'my-c++-mode-hook-tabs)


;;
;; C mode
(setq c-default-style "bsd"
      c-basic-offset 8)

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	  (column (c-langelem-2nd-pos c-syntactic-element))
	   (offset (- (1+ column) anchor))
	    (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(use-package ac-etags
  :ensure t
  :config
  (setq ac-etags-requires 1)
  :init
  (ac-etags-setup)
  (add-hook 'c-mode-hook
	    (lambda ()
	      (add-to-list 'ac-sources 'ac-source-etags))))


(use-package ac-c-headers
  :ensure t
  :config
  (add-hook 'c-mode-hook
	    (lambda ()
	      (add-to-list 'ac-sources 'ac-source-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-header-symbols t))))




(defun my-c-mode-hook-common ()
  (setq tab-width c-basic-offset)
  (setq c-auto-hungry-initial-state 'none)
  (setq c-delete-function 'delete-char)
  (setq c-tab-always-indent t)
;; BSD-ish indentation style

  (setq c-indent-level tab-width)
  (setq c-continued-statement-offset tab-width)
  (setq c-brace-offset (- 0 tab-width))
  (setq c-argdecl-indent 0)
  (setq c-label-offset (- 0 tab-width))


  (c-set-offset 'case-label '+)	     ; indent case labels by
				     ; c-indent-level, too
  ;; (whitespaces-mode)
)

;;
;; linux kernel codestyle
(defun my-c-mode-hook-tabs ()
  (my-c-mode-hook-common)
  (setq indent-tabs-mode t)

  (c-add-style
   "linux-tabs-only"
   '("linux" (c-offsets-alist
	      (arglist-cont-nonempty
	       c-lineup-gcc-asm-reg
	       c-lineup-arglist-tabs-only))))
  (c-set-style "linux-tabs-only")
)


(defun my-c-mode-hook-spaces ()
  (my-c-mode-hook-common)
  (setq indent-tabs-mode t)

  (c-set-style "linux")
)

(add-hook 'c-mode-hook 'my-c-mode-hook-tabs)
;; (add-hook 'c-mode-hook 'my-c-mode-hook-spaces)

;; Complement to next-error
(defun previous-error (n)
  "Visit previous compilation error message and corresponding source code."
  (interactive "p")
  (next-error (- n)))

(defun recompile()
  "default recompile function. It will be changed after the first call of compile function."
  (interactive)
  (compile compile-command))

;; set default compile command
(setq compile-command "make")


;; Misc...
(transient-mark-mode 1)
(setq mark-even-if-inactive t)
(setq visible-bell nil)
(setq next-line-add-newlines nil)
(setq suggest-key-bindings t)

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)


(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))
(menu-bar-mode -1)


;; Under UNIX
(if (not (equal system-type 'ms-dos))
    (progn
      (if first-time
      (server-start))))

;; Indicate that this file has been read at least once
(setq first-time nil)

;; No need to debug anything now

(setq debug-on-error nil)

;; All done
(message "All done, %s%s" (user-login-name) ".")



(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
                (replace-match (string ?\C-j) nil t))))


;; helper for Debian Website proofreading
(defun debwww-open-russian-file ()
  (interactive)
  (if (region-active-p) nil (mark-whole-buffer))
  (goto-char (region-beginning))
  (if (search-forward "> +++ russian/" nil t)
      (let ((filename (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (setq filename (replace-regexp-in-string "^.*+++ russian/" "russian/" filename))
        (setq filename (replace-regexp-in-string ".wml\t.*" ".wml" filename))
        (setq filename (concat "/mnt/data/developer/tmp/debian/webwml/" filename))
        (message "File is found: -%s-" filename)
        (deactivate-mark)
        (find-file-existing filename)
        )
    (message "WML filename is not found"))
  (deactivate-mark)
)

;; commit helper for Debian Website proofreading
(defun debwww-commit-changes ()
  (interactive)
  (shell-command "cvs diff")
  (switch-to-buffer-other-window "*Shell Command Output*")
  (if (equal (read-char-choice "Is diff correct? <n/Y>" '(?n ?y)) '?y)
      (shell-command "cvs commit -m '(Russian) Proofread translation'"))
  )


;;
;; Customize settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output (quote first-error))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (jedi epc ac-c-headers ac-etags projectile auto-complete ido-completing-read+ use-package find-file-in-project idle-highlight-mode ido-ubiquitous paredit smex solarized-theme magit xclip monky exec-path-from-shell)))
 '(safe-local-variable-values
   (quote
    ((whitespace-line-column . 80)
     (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
