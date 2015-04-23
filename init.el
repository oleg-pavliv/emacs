(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; (add-hook 'python-mode-hook 
;;           (lambda ()
;;             (setq indent-tabs-mode nil
;;                   tab-width 2)))

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))


;;backup files
(setq backup-directory-alist '(("." . "~/emacs-backup")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 10
  kept-old-versions 10
  version-control t)


;; is set because I dont use a default ls -l listing in dired and an error 'no file on this line' is thrown when I copy files in dired
(setq directory-listing-before-filename-regexp "[CD\\*]*[[:blank:]]*[rwxd-]+[[:blank:]]+[0-9a-zA-Z.]+[[:blank:]]+[0-9-]+[[:blank:]]+[0-9-:]+[[:blank:]]+")
;; (and
;;  (= 0 (string-match directory-listing-before-filename-regexp "  drwxrwxrwx       0 25-09-2012 14:48 scala-mode"))
;;  (= 38 (match-end 0))
;;  (= 0 (string-match directory-listing-before-filename-regexp "C drwxrwxrwx       0 25-09-2012 14:48 scala-mode"))
;;  (= 38 (match-end 0))
;;  (= 0 (string-match directory-listing-before-filename-regexp "  -rw-rw-rw-    4.2k 12-09-2012 13:50 org-custom.el"))
;;  (= 38 (match-end 0))
;;  (= 0 (string-match directory-listing-before-filename-regexp "* -rw-rw-rw-   12.6k  1-07-2013 08:17 init.el"))
;;  (= 38 (match-end 0)))


(require 'zone)
(zone-when-idle 600)
;; (setq zone-programs [zone-pgm-random-life])
(setq zone-programs [zone-pgm-drip-fretfully])

;; hide obsolete variables in emacs 23.3
;; from http://stackoverflow.com/questions/5468952/how-do-i-hide-emacs-obsolete-variable-warnings
(when (and (equal emacs-major-version 23) (equal emacs-minor-version 3))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars 'font-lock-beginning-of-syntax-function))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars 'font-lock-beginning-of-syntax-function)))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p) ;; make script file executable

(require 'whitespace)

;;(standard-display-ascii ?\t "^I")

(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))
(global-set-key "\M-/" 'hippie-expand)

;; restore the previous window config with C-c left. Redo with C-c right
(winner-mode 1)

;; move around the windows with Shift arrow
(windmove-default-keybindings)
(setq windmove-wrap-around t)

(setq undo-outer-limit 1000000)

(server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function) ;; to avoid an annoying message "Buffer buffername still has clients; kill it?"
;;(add-hook 'server-switch-hook 'raise-frame) ;; to pop-up the emacs frame on linux KDE (a tip from http://shreevatsa.wordpress.com/tag/emacs/ )

(setq history-delete-duplicates t)

(setq set-mark-command-repeat-pop t) ;; second and subsequent invocation of C-SPACE doesn't need a C-u

(setq dired-dnd-protocol-alist nil) ;; disable copy or move of file on drag-and-drop to dired mode

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function (if (eq system-type 'windows-nt) 'x-selection-value 'x-cut-buffer-or-selection-value))

(setq eval-expression-print-level 100)
(setq eval-expression-print-length 10000)

(setq comint-buffer-maximum-size 1024)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)


(setq kill-do-not-save-duplicates t)
(setq delete-by-moving-to-trash t)


(set-face-attribute 'default nil :height 130)

(when (string-equal "windows-nt" system-type)
  ;; (set-face-attribute 'default nil :family "Terminus")
  (set-face-attribute 'default nil :family "Andale Mono")
  ;; (set-face-attribute 'default nil :family "Consolas")
  )


(define-key (current-global-map) (kbd "C-c l") 'align)


(setq op:grep-find-cmd "find . -path '*/target' -prune -o -type f -maxdepth 1 -print0 | xargs -0 -e grep -n ")

(defun fgr (term)
  (interactive
   (list (read-string "grep-find: " (concat (replace-regexp-in-string "-maxdepth 1" "" op:grep-find-cmd)
                                            (if (use-region-p) (buffer-substring (region-beginning) (region-end)) "")))))
  (grep-find term))

(defun fg (term)
  (interactive
   (list (read-string "grep-find: " (concat op:grep-find-cmd (if (use-region-p) (buffer-substring (region-beginning) (region-end)) "")))))
  (grep-find term))


;;(setq debug-on-quit t)
;;(setq edebug-trac t)

(require 'ido)
(ido-mode t)

(defun op:set-coding-system (coding) ;; cp1251 for cyrilic
  (interactive "ZChoose coding system: ")
  (setq-default coding-system-for-read coding)
  (setq-default coding-system-for-write coding))

;; a problem with a zencoding-mode is a < prefix
;; (require 'zencoding-mode)
;; (add-hook 'sgml-mode-hook 'zencoding-mode)
;; (define-key zencoding-mode-keymap (kbd "<backtab>") 'zencoding-expand-line)

(require 'compile)

(add-to-list 'compilation-error-regexp-alist '("^[ \t]*\\([A-Za-z.0-9_: \\-]+\\)(\\([0-9]+\\)[,]\\( *[0-9]+\\))\\( Microsoft JScript runtime error\\| JSLINT\\): \\(.+\\)$" 1 2 3))

(add-hook 'js2-mode-hook (lambda () (setq compile-command
                                          (let ((file (file-name-nondirectory buffer-file-name)))
                                            (concat "cscript.exe " (getenv "home") "/emacs/scripts/jslint.js " file)))))

(add-hook 'c-mode-hook (lambda () (setq compile-command 
                                        (let* ((src (file-name-nondirectory buffer-file-name))
                                               (exe (first (split-string file "\\."))))
                                          (concat "gcc.exe -o "  exe " " src)))))


;; (iswitchb-mode 1)
;; (setq iswitchb-prompt-newbuffer nil) 

(setq calendar-week-start-day 1
      calendar-intermonth-text '(propertize
                                 (format "W%2d" (car (calendar-iso-from-absolute (calendar-absolute-from-gregorian (list month day year)))))
                                 'font-lock-face 'font-lock-function-name-face))

(when (string-equal "windows-nt" system-type)

  (setq dired-guess-shell-alist-user 
        (list
         (list "\\.zip$\\|\\.gz$\\|\\.tar$\\|\\.rar$\\|\\.jar$\\|\\.ear$\\|\\.aar$\\|\\.war$\\|\\.gzip$\\|\\.sar$\\|\\.bz2$" "7z x")))

  (add-to-list 'exec-path (concat (getenv "utils") "/7-Zip"))
  (setq archive-zip-extract '("7z" "x" "-so"))
  (setq archive-zip-expunge '("7z" "d" "-tzip"))
  (setq archive-zip-expunge '("7z" "d" "-ttar"))
  )


(setq dired-dwim-target t) ;; dired-do-copy checks if another dired buffer is present and put its dir as target

(setq grep-highlight-matches nil) ;; to avoid slowness when doing find-grep


(savehist-mode 1) ;; save minibuf history
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)) ;; not only minibuf but other things as well
;;(setq savehist-file "~/.emacs.d/savehist")


(defun op:init-frame (frame)
  (select-frame frame)
  (set-background-color "black")
  (set-foreground-color "grey50"))

;; set mode line colors to make the active window more visible
(set-face-attribute  'mode-line          nil  :foreground "black"  :background "ivory"  :box '(:line-width 1 :style released-button))
(set-face-attribute  'mode-line-inactive nil  :foreground "gray30" :background "black"  :box '(:line-width 1 :style released-button))

(op:init-frame (selected-frame))
(add-hook 'after-make-frame-functions 'op:init-frame)


(setq find-function-C-source-directory (concat (getenv "emacs_home") "/emacs-23.3/src"))

(setq large-file-warning-threshold 60000000)

(setq-default tab-width 4 indent-tabs-mode nil)
                                        ;(setq lisp-indent-function 'common-lisp-indent-function)

(setq ls-lisp-format-time-list (quote ("%e-%m-%Y %H:%M" "%e-%m-%Y %H:%M")))
(setq ls-lisp-use-localized-time-format 1)

(setq dired-recursive-deletes 'top)

(global-auto-revert-mode 1)

(column-number-mode 1)
(line-number-mode 1)
(size-indication-mode 1)
(setq mode-line-position '((-3 "%p")  ;; show size/SIZE 
                           (size-indication-mode (8 " of %i/%I")) 
                           (line-number-mode ((column-number-mode (10 " (%l,%c)") (6 " L%l"))) ((column-number-mode (5 " C%c"))))))

(font-lock-mode 1)


;;(c-subword-mode 1)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(transient-mark-mode 1)
(setq mark-even-if-inactive nil) ;; to not kill region with C-w if it is not active

;; does not work in emacs 24.1
;; (partial-completion-mode)

(set-keyboard-coding-system 'latin-1)

;; does not work in emacs 24.1
;; (setq-default enable-multibyte-characters 1)

(setq max-lisp-eval-depth 10000)

(setq line-number-display-limit nil) 
(setq line-number-display-limit-width 10000)

(time-stamp)

(grep-compute-defaults)


;; (require 'cl)
;; (add-to-list 'load-path "D:/Soft/emacs/emacs-tiny-tools/lisp/tiny")
;; (add-to-list 'load-path "D:/Soft/emacs/emacs-tiny-tools/lisp/other")
;; (add-to-list 'load-path "D:/Soft/emacs/emacs-tiny-tools/lisp")
;; (load "tinypath")

;; use bash shell
(when (string-equal "windows-nt" system-type) 
  ;;  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)
  (setq explicit-shell-file-name (concat (getenv "CYGWIN_HOME") "/bin/bash") )
  (setq shell-file-name explicit-shell-file-name))

(define-key global-map (kbd "C-c C-g") (lambda (arg) (interactive "P") (yas/exit-all-snippets)))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (setq org-startup-folded t)
(setq org-tags-column 80)

(define-key global-map "\C-ct" 'org-todo-list)
(define-key global-map "\C-ca" 'org-agenda)

;; (define-key global-map "\M-t" 'transpose-lines)

;; (when (string-equal "windows-nt" system-type)
;;   (add-to-list 'exec-path (concat (getenv "PERL_HOME") "/bin")))

(add-to-list 'exec-path (concat (getenv "utils") "/gnuplot/bin"))

(cua-selection-mode 1)

(setq cursor-type 'bar)
(blink-cursor-mode 1)

;; to get a printer name look how it is shown in the txtpad and replace back slash with forward slash
(setq printer-name "//chx-prnt-01/CHX_PRINT")

(define-key (current-global-map) (kbd "C-x C-b") 'buffer-menu)

(define-key (current-global-map) (kbd "C-x C-i") 'hexl-find-file)

;; (unless (string-equal "windows-nt" system-type)
;;   (require 'tramp)
;;   (setq tramp-default-method "telnet"))

;; (setq tramp-default-method "ssh")
;; (setq tramp-default-user "lysis")
;; (setq auth-sources `((:source ,(concat (getenv "home") "/emacs/authinfo") :host t :protocol t)))


(setq ediff-diff-options "-w")
                                        ;(setq-default ediff-ignore-similar-regions t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(global-unset-key "\C-z")

(autoload 'fi:common-lisp "fi-site-init" "" t)

(define-key (current-global-map) (kbd "C-c -") (lambda (kill-line) 
                                                 (interactive "P")
                                                 (when kill-line 
                                                   (move-end-of-line nil) 
                                                   (kill-line)) 
                                                 (delete-horizontal-space)
                                                 (insert " ")))


(setenv "PATH" (concat (getenv "home") "/emacs/scripts;" (getenv "PATH")))


(add-hook 'find-file-hooks (lambda () 
                             (when (> (buffer-size) (* 1024 1024))
                               (setq buffer-read-only t)
                               (buffer-disable-undo)
                               (message "Buffer is set to read-only because it is large.  Undo also disabled."))))

;; (setq bookmark-save-flag 1)



(require 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("java" (mode . java-mode))
               ("shell" (mode . sh-mode))
               ("org" (mode . org-mode))
               ("sqlplus" (mode . sqlplus-mode))
               ("sql" (mode . sql-mode))
               ("xml" (mode . nxml-mode))
               ("el" (mode . emacs-lisp-mode))
               ))))


(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook 
          (lambda () 
            (ibuffer-switch-to-saved-filter-groups "default")))

(define-key global-map (kbd "C-x C-b") (lambda () (interactive) (ibuffer-other-window) (delete-other-windows)))
;; (setq ibuffer-default-sorting-mode 'major-mode)


;; add the following variable to the .emacs to have 4 spaces for the TAB in the text mode
;; (custom-set-variables
;;  '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
;; )




;;

(require 'ediff-vers)

(define-key vc-prefix-map "=" (lambda ()
            (interactive)
            (if (equal 'dired-mode major-mode)
                (dired-find-file))
            (if (equal 'magit-status-mode major-mode)
                (magit-visit-item))
            (ediff-vc-internal "" "")))
