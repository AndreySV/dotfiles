;; -*-Emacs-Lisp-*-

;; This file is designed to be re-evaled; use the variable first-time
;; to avoid any problems with this.
(defvar first-time t
  "Flag signifying this is the first time that .emacs has been evaled")

;; UTF-8 as default encoding
(set-language-environment 'UTF-8)
(set-default-coding-systems 'utf-8)

;;
;; A secure Emacs environment
(setq tls-checktrust t)
(setq gnutls-verify-error t)
;; gnutls-trustfile is already set to correct value in Debian GNU/Linux
;; (setq gnutls-trustfiles (list trustfile))

;; Test the settings by using the following code snippet:
;; (let ((bad-hosts
;;        (loop for bad
;;              in `("https://wrong.host.badssl.com/"
;;                   "https://self-signed.badssl.com/")
;;              if (condition-case e
;;                     (url-retrieve
;;                      bad (lambda (retrieved) t))
;;                   (error nil))
;;              collect bad)))
;;   (if bad-hosts
;;       (error (format "tls misconfigured; retrieved %s ok"
;;                      bad-hosts))
;;     (url-retrieve "https://badssl.com"
;;                   (lambda (retrieved) t))))


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
;; Customize settings
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :noerror)

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
  :ensure t
  :config
    (global-set-key (kbd "C-c m") 'magit-status))


;;
;; similar to magit packages for mercurial
(use-package monky
  :ensure t
  :config
  (global-set-key (kbd "C-c o") 'monky-status))

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

;; typing an open parenthesis automatically inserts
;; the corresponding closing parenthesis
(electric-pair-mode t)

;;
;; increase selected region by semantic units.
(use-package expand-region
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
(global-set-key [f6]   'next-error)
(global-set-key [S-f6] 'previous-error)
(global-set-key [f5]   'recompile)
(global-set-key [S-f5] 'compile)


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



;; Mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

;; Move to trash when deleting files
(setq delete-by-moving-to-trash t)


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
(setq emacs-autosave-directory (concat user-emacs-directory "autosave/"))

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


;; Misc...
(transient-mark-mode 1)
(setq mark-even-if-inactive t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq next-line-add-newlines nil)
(setq suggest-key-bindings t)

;; Appearance
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))
(menu-bar-mode -1)

;; Treat 'y' or <CR> as yes, 'n' as no.
(defalias 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;;
;; Load/Save emacs session (openned files) and restore buffers
(require 'desktop)
(setq desktop-save t)			; always save and never ask
(setq desktop-load-locked-desktop t)	; load even locked desktop, in case emacs crashed
(desktop-save-mode 1)

(require 'tar-mode)
(require 'dired)

;; for isearch-forward, make these equivalent: space newline tab hyphen underscore
(setq search-whitespace-regexp "[-_ \t\n]+")

;;
;; Text mode

;; automatically break lines at spaces
;; when the line becomes too wide.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;
;; enable spellchecker
(eval-after-load "ispell"
  '(when (executable-find ispell-program-name)
     (add-hook 'text-mode-hook 'turn-on-flyspell)))


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



;;
;; Python mode

;; external python packages are required to use jedi
;;
;; For Debian GNU/Linux systems these are:
;; apt-get install python-sexpdata python-epc python-jedi
;;
(use-package jedi
  :ensure t
  :config

  ;; Global Jedi config vars

  (defvar jedi-config:use-system-python nil
    "Will use system python and active environment for Jedi server.
    May be necessary for some GUI environments (e.g., Mac OS X)")

  (defvar jedi-config:with-virtualenv nil
    "Set to non-nil to point to a particular virtualenv.")

  (defvar jedi-config:vcs-root-sentinel '(".hg" ".git"))

  (defvar jedi-config:python-module-sentinel "__init__.py")

  ;; (Many) config helpers follow

  ;; Alternative methods of finding the current project root
  ;; Method 1: basic
  (defun get-project-root (buf repo-files &optional init-file)
    "Just uses the vc-find-root function to figure out the project root.
       Won't always work for some directory layouts."
    (let* ((buf-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
	   (project-root nil))
      (progn
	(dolist (repo-file repo-files)
	  (setq project-root (vc-find-root buf-dir repo-file))
	  (when project-root (return)))
	(if project-root
	    (expand-file-name project-root)
	  nil))))

  ;; Set this variable to find project root
  (defvar jedi-config:find-root-function 'get-project-root)

  (defun current-buffer-project-root ()
    (funcall jedi-config:find-root-function
	     (current-buffer)
	     jedi-config:vcs-root-sentinel
	     jedi-config:python-module-sentinel))

  (defun jedi-config:setup-server-args ()
    ;; little helper macro for building the arglist
    (defmacro add-args (arg-list arg-name arg-value)
      `(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))
    ;; and now define the args
    (let ((project-root (current-buffer-project-root)))

      (make-local-variable 'jedi:server-args)

      (when project-root
	(message (format "Adding system path: %s" project-root))
	(add-args jedi:server-args "--sys-path" project-root))

      (when jedi-config:with-virtualenv
	(message (format "Adding virtualenv: %s" jedi-config:with-virtualenv))
	(add-args jedi:server-args "--virtual-env" jedi-config:with-virtualenv))))

  ;; Hook up to auto-complete
  (add-to-list `ac-sources `ac-source-jedi-direct)
  ;; Enable for python-mode
  (add-hook `python-mode-hook `jedi:setup)
  ;; Buffer-specific server options
  (add-hook 'python-mode-hook 'jedi-config:setup-server-args)
  ;; Start completion at method dot
  (setq jedi:complete-on-dot t)
  )


;;
;; Sedona language
(if first-time
    (setq auto-mode-alist
      (append '(("\\.sedona$" . c++-mode)
	    ) auto-mode-alist)))


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




;;
;; user's helper functions
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
