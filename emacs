;; -*-Emacs-Lisp-*-

;; This file is designed to be re-evaled; use the variable first-time
;; to avoid any problems with this.
(defvar first-time t
  "Flag signifying this is the first time that .emacs has been evaled")

(set-language-environment 'UTF-8)


;;
;; repositories
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
      ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
  )


;;
;; enable ido completion everywhere
(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

;; allows to use ido for completion of commands in M-x,
;; with enhancements like putting your most-used commands
;; at the front of the list
(require 'smex)
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.

;; replace normal M-x key-binding with smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)



;;
;; Meta
(global-set-key "\M- " 'set-mark-command)
(global-set-key "\M-h" 'backward-kill-word)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-\C-r" 'query-replace)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-g" 'goto-line)


;; Function keys
(global-set-key [f1] 'manual-entry)
(global-set-key [f2] 'info)
(global-set-key [f3] 'repeat-complex-command)
(global-set-key [f4] 'advertised-undo)
(global-set-key [f5] 'eval-current-buffer)
(global-set-key [f6] 'buffer-menu)
(global-set-key [f7] 'other-window)
(global-set-key [f8] 'find-file)
(global-set-key [f9] 'save-buffer)

;; (global-set-key [f6] 'next-error)
;; (global-set-key [S-f6] 'previous-error)

;; (global-set-key [C-f9] 'gdb)
;; (global-set-key [f9] 'compile)
;; (global-set-key [f10] 'save-buffers-kill-terminal)
(global-set-key [f10] 'next-error)
(global-set-key [S-f10] 'previous-error)
(global-set-key [f11] 'recompile)
(global-set-key [S-f11] 'compile)
(global-set-key [f12] 'grep)
(global-set-key [C-f1] 'compile)
(global-set-key [C-f2] 'grep)
(global-set-key [C-f3] 'next-error)
(global-set-key [C-f5] 'display-faces)
(global-set-key [C-f8] 'dired)
(global-set-key [C-f10] 'kill-compilation)


;
;; Keypad bindings
(global-set-key [up] "\C-p")
(global-set-key [down] "\C-n")
(global-set-key [left] "\C-b")
(global-set-key [right] "\C-f")
(global-set-key [home] "\C-a")
(global-set-key [end] "\C-e")
(global-set-key [prior] "\M-v")
(global-set-key [next] "\C-v")
(global-set-key [C-up] "\M-\C-b")
(global-set-key [C-down] "\M-\C-f")
(global-set-key [C-left] "\M-b")
(global-set-key [C-right] "\M-f")
(global-set-key [C-home] "\M-<")
(global-set-key [C-end] "\M->")
(global-set-key [C-prior] "\M-<")
(global-set-key [C-next] "\M->")



;; Mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

;; 
;; save user data separe from configuration directory
(setq emacs-user-directory "~/.emacs_usr")
(if (not (file-exists-p emacs-user-directory))
    (make-directory emacs-user-directory t))


(require 'xclip)
(xclip-mode +1)
(setq
  x-select-enable-clipboard t
  x-select-enable-primary t
  x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
  x-stretch-cursor t)


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

(setq emacs-backup-directory (concat emacs-user-directory "/backup"))

;; autocreate backup directory
(if (not (file-exists-p emacs-backup-directory))
    (make-directory emacs-backup-directory t))

;; delete backup files older than a week on start
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files emacs-backup-directory t))
    (when (and (backup-file-name-p file)
	       (> (- current (float-time (nth 5 (file-attributes file))))
		  week))
      (message "%s" file)
      (delete-file file))))


(setq backup-directory-alist
      `((".*" . ,emacs-backup-directory)))







;; 
;; autosave

;; path for autosave files
(setq emacs-autosave-directory (concat emacs-user-directory "/autosave"))

;; autocreate directory for autosave
(if (not (file-exists-p emacs-autosave-directory))
    (make-directory emacs-autosave-directory t))

;; delete autosave files older than a week on start
(message "Deleting old autosave files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files emacs-autosave-directory t))
    (when (and (backup-file-name-p file)
	       (> (- current (float-time (nth 5 (file-attributes file))))
		  week))
      (message "%s" file)
      (delete-file file))))

(setq auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))





;; autodetection of codepage
(when (load "auto-enca" 'noerror)
  (modify-coding-system-alist 'file "" 'enca-detect-coding))


;; command history
(setq savehist-file (concat emacs-user-directory "/savehist"))
(savehist-mode t)




;; checkpatch for C source files
(defun checkpatch()
  (interactive)
  (compile (concat "checkpatch.pl --emacs --terse --file " (buffer-file-name))))


;; iBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; Mouse
(global-set-key [mouse-3] 'imenu)

;; Misc
(global-set-key [C-tab] "\C-q\t")   ; Control tab quotes a tab.
(setq backup-by-copying-when-mismatch t)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
    (define-key query-replace-map [return] 'act)
    (define-key query-replace-map [?\C-m] 'act)

;; Load packages
(require 'desktop)
(require 'tar-mode)


;; Pretty diff mode
(autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files-remote "ediff"
  "Intelligent Emacs interface to diff")

(if first-time
    (setq auto-mode-alist
      (append '(("\\.cpp$" . c++-mode)
	    ("\\.hpp$" . c++-mode)
		    ("\\.lsp$" . lisp-mode)
	    ("\\.scm$" . scheme-mode)
	    ("\\.pl$" . perl-mode)
	    ) auto-mode-alist)))

;; Auto font lock mode
(defvar font-lock-auto-mode-list
  (list 'c-mode 'c++-mode 'c++-c-mode 'emacs-lisp-mode 'lisp-mode 'perl-mode 'scheme-mode)
  "List of modes to always start in font-lock-mode")

(defvar font-lock-mode-keyword-alist
  '((c++-c-mode . c-font-lock-keywords)
    (perl-mode . perl-font-lock-keywords))
  "Associations between modes and keywords")

(defun font-lock-auto-mode-select ()
  "Automatically select font-lock-mode if the current major mode is
in font-lock-auto-mode-list"
  (if (memq major-mode font-lock-auto-mode-list)
      (progn
    (font-lock-mode t))
    )
  )

(global-set-key [M-f1] 'font-lock-fontify-buffer)

;; New dabbrev stuff
;(require 'new-dabbrev)
(setq dabbrev-always-check-other-buffers t)
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
(add-hook 'emacs-lisp-mode-hook
      '(lambda ()
	 (set (make-local-variable 'dabbrev-case-fold-search) nil)
	 (set (make-local-variable 'dabbrev-case-replace) nil)))
(add-hook 'c-mode-hook
      '(lambda ()
	 (set (make-local-variable 'dabbrev-case-fold-search) nil)
	 (set (make-local-variable 'dabbrev-case-replace) nil)))
(add-hook 'text-mode-hook
      '(lambda ()
	 (set (make-local-variable 'dabbrev-case-fold-search) t)
	 (set (make-local-variable 'dabbrev-case-replace) t)))



;; C++ and C mode...
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


(defun my-c++-mode-hook-common ()
  (setq tab-width c-basic-offset)
  (define-key c++-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key c++-mode-map "\C-ce" 'c-comment-edit)
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






(defun my-c-mode-hook-common ()
  (setq tab-width c-basic-offset)
  (define-key c-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key c-mode-map "\C-ce" 'c-comment-edit)
  (setq c-auto-hungry-initial-state 'none)
  (setq c-delete-function 'delete-char)

  (define-key c-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key c-mode-map "\C-ce" 'c-comment-edit)
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

;; Perl mode
(defun my-perl-mode-hook ()
  (setq tab-width 4)
  (define-key c++-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (setq perl-indent-level 4)
  (setq perl-continued-statement-offset 4))


;; XML mode

;; hide-show for nxml mode
(add-to-list 'hs-special-modes-alist
	     '(nxml-mode
	       "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
	       ""
	       "<!--" ;; won't work on its own; uses syntax table
	       (lambda (arg) (my-nxml-forward-element))
	       nil))

(defun my-nxml-forward-element ()
  (let ((nxml-sexp-element-flag))
    (setq nxml-sexp-element-flag (not (looking-at "<!--")))
    (unless (looking-at outline-regexp)
      (condition-case nil
	  (nxml-forward-balanced-item 1)
	(error nil)))))

;; where to find xml schemas
;; (eval-after-load 'rng-loc
  ;; '(add-to-list 'rng-schema-locating-files "~/.schema/schemas.xml"))




;; Scheme mode...
(defun my-scheme-mode-hook ()
  (define-key scheme-mode-map "\C-m" 'reindent-then-newline-and-indent))

;; emacs-lisp mode...
(defun my-lisp-mode-hook ()
  (define-key lisp-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key lisp-mode-map "\C-i" 'lisp-indent-line)
  (define-key lisp-mode-map "\C-j" 'eval-print-last-sexp))

;; Add all of the hooks...
(add-hook 'c++-mode-hook 'my-c++-mode-hook-tabs)
(add-hook 'c-mode-hook 'my-c-mode-hook-tabs)
;; (add-hook 'c-mode-hook 'my-c-mode-hook-spaces)
(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'perl-mode-hook 'my-perl-mode-hook)

;; Complement to next-error
(defun previous-error (n)
  "Visit previous compilation error message and corresponding source code."
  (interactive "p")
  (next-error (- n)))

(defun recompile()
  "default recompile function. It will be changed after the first call of compile function."
  (interactive)
  (compile compile-command))



;; Misc...
(transient-mark-mode 1)
(setq mark-even-if-inactive t)
(setq visible-bell nil)
(setq next-line-add-newlines nil)
(setq compile-command "make")

(setq suggest-key-bindings nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Elisp archive searching
(autoload 'format-lisp-code-directory "lispdir" nil t)
(autoload 'lisp-dir-apropos "lispdir" nil t)
(autoload 'lisp-dir-retrieve "lispdir" nil t)
(autoload 'lisp-dir-verify "lispdir" nil t)

;; Font lock mode
(defun my-make-face (face colour &optional bold)
  "Create a face from a colour and optionally make it bold"
  (make-face face)
  (copy-face 'default face)
  (set-face-foreground face colour)
  (if bold (make-face-bold face))
  )

(if (eq window-system 'x)
    (progn
      (my-make-face 'blue "blue")
      (my-make-face 'red "red")
      (my-make-face 'green "dark green")
      (setq font-lock-comment-face 'blue)
      (setq font-lock-string-face 'bold)
      (setq font-lock-type-face 'bold)
      (setq font-lock-keyword-face 'bold)
      (setq font-lock-function-name-face 'red)
      (setq font-lock-doc-string-face 'green)
      (add-hook 'find-file-hooks 'font-lock-auto-mode-select)

      (setq baud-rate 1000000)
      (global-set-key "\C-cmm" 'menu-bar-mode)
      (global-set-key "\C-cms" 'scroll-bar-mode)
      (global-set-key [backspace] 'backward-delete-char)
		    ;	   (global-set-key [delete] 'delete-char)
      (standard-display-european t)
      (load-library "iso-transl")))

;; X11 or PC using direct screen writes
(if window-system
    (progn
      ;;      (global-set-key [M-f1] 'hilit-repaint-command)
      ;;      (global-set-key [M-f2] [?\C-u M-f1])
      (setq hilit-mode-enable-list
	'(not text-mode c-mode c++-mode emacs-lisp-mode lisp-mode
	  scheme-mode)
	hilit-auto-highlight nil
	hilit-auto-rehighlight 'visible
	hilit-inhibit-hooks nil
	hilit-inhibit-rebinding t)
      (require 'hilit19)
      (require 'paren))
  (setq baud-rate 2400)		; For slow serial connections
  )

;; TTY type terminal
(if (and (not window-system)
     (not (equal system-type 'ms-dos)))
    (progn
      (if first-time
      (progn
	(keyboard-translate ?\C-h ?\C-?)
	(keyboard-translate ?\C-? ?\C-h)))))

;; Under UNIX
(if (not (equal system-type 'ms-dos))
    (progn
      (if first-time
      (server-start))))

;; Add any face changes here
(add-hook 'term-setup-hook 'my-term-setup-hook)
(defun my-term-setup-hook ()
  (if (eq window-system 'pc)
      (progn
;;  (set-face-background 'default "red")
    )))

;; Restore the "desktop" - do this as late as possible
(if first-time
    (progn
      (desktop-load-default)
      (desktop-read)))

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


;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
	If no region is selected and current line is not blank and we are not at the end of the line,
	then comment current line.
	Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key "\M-;" 'comment-dwim-line)
(put 'downcase-region 'disabled nil)
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
    (find-file-in-project idle-highlight-mode ido-ubiquitous paredit smex solarized-theme magit xclip pymacs monky exec-path-from-shell)))
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
