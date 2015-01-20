(require 'time-stamp)

;; disable tramp to avoid delays in opening files
(setq tramp-mode nil)

(setq org-confirm-babel-evaluate nil)

(add-to-list 'load-path (concat (getenv "home") "/emacs/addons"))

(toggle-uniquify-buffer-names)

;; log initialization time per file
(let ((log-file (concat (getenv "TEMP") "/emacs.log")))
  (if (file-exists-p log-file)
    (delete-file log-file)))

(defun write-string-to-file (string file)
   (with-temp-buffer
     (insert (concat string "\n"))
       (append-to-file (point-min) (point-max) file)))

(defun op:log (msg)
  (let ((wmsg (concat (time-stamp-string) "    " msg)))
    (message wmsg)
    (write-string-to-file wmsg (concat (getenv "TEMP") "/emacs.log"))))


;; fast moving on the page. press C-c SPC and a first letter of a word
;; C-u C-c SPC       ==> ace-jump-char-mode
;; C-u C-u C-c SPC   ==> ace-jump-line-mode
(op:log "loading ace-jump-mode")
(require 'ace-jump-mode "~/emacs/addons/ace-jump-mode.el")
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;; (op:log "loading undo-tree")
;; (require 'undo-tree "~/emacs/addons/undo-tree.el")
;; (global-undo-tree-mode)


;;---------------- evil -----------------------
;; (add-to-list 'load-path (concat (getenv "emacs_home") "/evil"))
;; (require 'evil)
;; (evil-mode 1)
;; (setq evil-move-cursor-back nil)

;; (evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state 'shell-mode 'emacs)
;; (evil-set-initial-state 'org-mode 'emacs)

;; (define-key global-map (kbd "C-c v") 'evil-local-mode)
;; (define-key global-map (kbd "C-c C-v") 'evil-mode)
;; ;; escape cancels everything
;; (define-key evil-normal-state-map [escape] 'keyboard-quit)
;; (define-key evil-visual-state-map [escape] 'keyboard-quit)
;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; (define-key evil-insert-state-map (kbd "C-n") 'next-line)
;; (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
;; (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
;; (define-key evil-insert-state-map (kbd "C-k") 'delete-line)
;; (define-key evil-insert-state-map (kbd "C-f") 'forward-char)
;; (define-key evil-insert-state-map (kbd "C-y") 'yank)

;; to define commands use the following
;; (evil-ex-define-cmd "e[dit]" 'find-file)
;; or even better
;; (define-key evil-ex-map "e" 'find-file)

;;---------------- rarely used stuff -----------------------
;; (load-file (expand-file-name "~/emacs/addons/js2.elc"))
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(op:log "loading yasnippet")
(require 'yasnippet "~/emacs/addons/yasnippet.el")
(yas/initialize)
(setq yas/root-directory "~/emacs/snippets")
(yas/load-directory yas/root-directory)


;; (autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;; (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
;; (add-hook 'php-mode-hook '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

;;(require 'php-mode)


;;----------------------------------------------------------
;; (load-file (expand-file-name "~/emacs/addons/multi-web-mode/multi-web-mode.el"))

;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;                   (espresso-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;                   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)


;;----------------------------------------------------------


;; (setq load-path (cons "~/emacs/addons/apel-10.8/" load-path))
;; (load-file (expand-file-name "~/emacs/addons/elscreen.el"))
;; (load "elscreen" "ElScreen" t)

(op:log "loading ioccur")
(require 'ioccur "~/emacs/addons/ioccur.elc")

(define-key ioccur-mode-map (kbd "<left>") nil)
(define-key ioccur-mode-map (kbd "<right>") nil)
(define-key global-map (kbd "C-c i") 'ioccur)

(op:log "loading stripes mode")
(require 'stripes "~/emacs/addons/stripes.el")


(op:log "loading browse-kill-ring")
(load-file (expand-file-name "~/emacs/addons/browse-kill-ring.el"))
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "C-c k") 'browse-kill-ring)


;; hl-tags-mode highlights html tags
;; a function hl-tags-show has been modified to use paren-face-match instead of show-paren-match-face
;; (require 'hl-tags-mode "~/emacs/addons/hl-tags-mode.el")
;; (add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
;; (add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))


(require 'bar-cursor "~/emacs/addons/bar-cursor.el")
(bar-cursor-mode 1)

;;(load-file (expand-file-name "~/emacs/addons/zencoding-mode.el"))

(op:log "loading markerpen and LRU-yank")
(load-file (expand-file-name "~/emacs/addons/markerpen.el"))
;; (load-file (expand-file-name "~/emacs/addons/key-chord.el"))
(load-file (expand-file-name "~/emacs/addons/LRU-yank.el"))


;; iedit change all occurences of a string
(op:log "loading iedit")
(load-file (expand-file-name "~/emacs/addons/iedit.el"))
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)


;; (add-to-list 'load-path "~/emacs/sanityinc-color-theme")
;; (require 'color-theme-sanityinc-tomorrow)


;; (add-to-list 'load-path (concat (getenv "emacs_home") "/color-theme-6.6.0"))
;; (load-file (concat (getenv "emacs_home") "/color-theme-6.6.0/color-theme.el"))
;; (require 'color-theme)
;; (eval-after-load "color-theme"  '(progn  (color-theme-initialize) (color-theme-hober)))


(load-file (expand-file-name "~/emacs/addons/bubble-buffer.el"))




(when (string-equal "windows-nt" system-type)
  (op:log "loading eimp")
  ;; image manipulation program, uses mogrify
  (load-file (expand-file-name "~/emacs/addons/eimp.el")))

;; (load-file (expand-file-name "~/emacs/ediff-trees.el"))
;; (global-set-key (kbd "s-SPC") 'ediff-trees-examine-next)
;; (global-set-key (kbd "S-s-SPC") 'ediff-trees-examine-previous)
;; (global-set-key (kbd "C-s-SPC") 'ediff-trees-examine-next-regexp)
;; (global-set-key (kbd "C-S-s-SPC") 'ediff-trees-examine-previous-regexp)


(when (getenv "cygwin_home")
  (op:log "loading cygwin")
  (require 'cygwin-mount "~/emacs/addons/cygwin-mount.el")
  (cygwin-mount-activate)
  (setenv "PATH" (concat (getenv "cygwin_home") "/bin;" (getenv "PATH")))
  (setq exec-path (cons (concat (getenv "cygwin_home") "/bin;") exec-path))
)



(op:log "loading init.el")
(load-file (expand-file-name "~/emacs/init.el"))

(setq nxml-child-indent 4)

(op:log "loading nxml-custom.el")
(load-file (expand-file-name "~/emacs/nxml-custom.el"))


(op:log "loading misc.el")
(load-file (expand-file-name "~/emacs/misc.el"))

;; (add-hook 'window-setup-hook
;;           '(lambda() 
;;             (progn 
;;               (set-frame-position (selected-frame) 0 0)
;;               (set-frame-height (selected-frame) 60)
;;               (set-frame-width (selected-frame) 180))))


;; (add-to-list 'default-frame-alist '(height . 60))
;; (add-to-list 'default-frame-alist '(width . 180))
;; (setq initial-frame-alist default-frame-alist)
;; (setq special-display-frame-alist default-frame-alist)



(op:log "loading sqlplus")
(load-file (expand-file-name "~/emacs/addons/sqlplus.el"))
(op:log "loading sqlplus-custom.el")
(load-file (expand-file-name "~/emacs/sqlplus-custom.el"))

(op:log "loading hideshow.el")
(load-library "hideshow")
(setq hs-isearch-open nil)
;; (add-hook 'java-mode-hook 'hs-minor-mode)
;; (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(op:log "loading micparen.el")
(load-file (expand-file-name "~/emacs/addons/mic-paren.elc"))
(setq paren-dont-touch-blink t)
(require 'mic-paren)
(paren-activate)
(setq paren-match-face 'highlight)
(setq paren-sexp-mode t)

(op:log "loading paredit.el")
(load-file (expand-file-name "~/emacs/addons/paredit-beta.el"))
(mapc (lambda (mode)
	(let ((hook (intern (concat (symbol-name mode)
				    "-mode-hook"))))
	  (add-hook hook (lambda () (paredit-mode +1)))))
      '(emacs-lisp lisp inferior-lisp))


(when (getenv "NAGRA")
  (op:log "loading nagra.el")
  (load-file (expand-file-name "~/emacs/nagra.el")))

(when (getenv "NAGRA")
  (op:log "loading p4")
  (load-file (expand-file-name "~/emacs/addons/p4.el"))
  (load-library "p4")
  (op:log "loading p4-custom")
  (load-file (expand-file-name "~/emacs/p4-custom.el")))


;; the following line adds op:sql-try-expand-column to the hippie expand. It is disabled now because sql-custom is not loaded
;; '(hippie-expand-try-functions-list (quote (op:sql-try-expand-column try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol)))

(op:log "custom-set-variables")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-kill-ring-display-duplicates nil)
 '(browse-kill-ring-maximum-display-length 200)
 '(calendar-intermonth-spacing 6)
 '(calendar-left-margin 8)
 '(confirm-kill-emacs nil)
 '(dabbrev-expand-multiple-select-keys (quote ("a" "s" "d" "f" "j" "k" "l" "é")))
 '(dabbrev-limit nil)
 '(dabbrev-search-these-buffers-only nil)
 '(diff-command "diff")
 '(dired-listing-switches "-lA")
 '(dired-recursive-deletes (quote always))
 '(global-auto-revert-ignore-modes (quote (jboss-log-mode)))
 '(gutter-buffers-tab-visible-p nil)
 '(hippie-expand-try-functions-list (quote (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(line-move-visual nil)
 '(message-log-max 1000)
 '(org-agenda-remove-tags nil)
 '(org-agenda-sorting-strategy (quote ((agenda todo-state-up priority-down category-up) (todo priority-down category-keep) (tags priority-down category-keep) (search category-keep))))
 '(org-agenda-tags-column -100)
 '(org-agenda-todo-keyword-format "%-12s")
 '(org-deadline-warning-days 0)
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "code" "log" "config")))
 '(org-global-properties (quote (("Owner_ALL" . "oleg cme jml"))))
 '(org-startup-folded (quote content))
 '(org-tag-alist (quote ((#("oleg" 0 4 (face nil)) . 111) (#("delegated" 0 9 (face nil)) . 100))))
 '(org-tags-column 100)
 '(org-todo-keywords (quote ((sequence "TODO" "STANDBY" "DONE") (sequence "CANCELED") (sequence "POST-It" "TRASH"))))
 '(pabbrev-global-mode-buffer-size-limit 10000)
 '(paren-match-face (quote paren-face-match))
 '(safe-local-variable-values (quote ((Package . common-lisp-user) (package . user))))
 '(split-width-threshold 120)
 '(sqlplus-format-output-tables-flag t)
 '(sqlplus-output-buffer-max-size 100000)
 '(sqlplus-save-passwords t)
 '(sqlplus-select-result-max-col-width 150)
 '(sr-show-hidden-files t)
 '(tags-add-tables nil)
 '(time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")
 '(toolbar-visible-p nil)
 '(zencoding-preview-default t))





(custom-set-faces
 '(compilation-info ((((class color) (min-colors 88) (background dark)) (:foreground "Green4"))))
 '(completions-first-difference ((t (:inherit bold :foreground "Orange" :weight bold))))
 '(cursor ((t (:foreground "yellow"))))
 '(custom-comment ((((class grayscale color) (background dark)) (:background "gray30" :foreground "white"))))
 '(custom-comment-tag ((((class color) (background dark)) (:foreground "DarkGreen"))))
 '(custom-state ((((class color) (background dark)) (:foreground "lime green"))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :weight bold))))
 '(dired-header ((t (:inherit font-lock-type-face :weight bold))))
 '(ediff-current-diff-A ((t (:background "slate gray" :foreground "azure"))))
 '(ediff-current-diff-B ((t (:background "slate gray" :foreground "azure"))))
 '(escape-glyph ((((background dark)) (:foreground "pink"))))
 '(font-lock-builtin-face ((((class color) (min-colors 88) (background dark)) (:inherit font-lock-function-name-face))))
 '(font-lock-comment-face ((nil (:foreground "DarkSeaGreen4"))))
 '(font-lock-constant-face ((((class color) (min-colors 88) (background dark)) (:foreground "gray50"))))
 '(font-lock-doc-face ((t (:foreground "DarkSeaGreen4"))))
 '(font-lock-doc-string-face ((((class color) (background light)) (:foreground "CadetBlue"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "LemonChiffon3"))))
 '(font-lock-jl-key-1-face ((t (:foreground "YellowGreen"))))
 '(font-lock-jl-key-2-face ((t (:foreground "green4"))))
 '(font-lock-jl-key-3-face ((t (:foreground "snow"))))
 '(font-lock-jl-key-face ((t (:foreground "firebrick"))))
 '(font-lock-jl-line-1-face ((t (:foreground "IndianRed"))))
 '(font-lock-jl-line-2-face ((t (:foreground "LemonChiffon3"))))
 '(font-lock-jl-line-3-face ((t (:foreground "yellow"))))
 '(font-lock-keyword-face ((t (:underline t))))
 '(font-lock-preprocessor-face ((t (:underline t))))
 '(font-lock-reference-face ((((class color) (background light)) (:foreground "darkblue"))))
 '(font-lock-string-face ((nil (:foreground "grey"))) nil "nnn")
 '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "gray50"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) nil)))
 '(hi-green ((((min-colors 88)) (:foreground "DarkSeaGreen"))))
 '(hi-green-bg ((((min-colors 88) (background dark)) (:background "PaleGreen" :foreground "black"))))
 '(hi-pink ((((min-colors 88)) (:foreground "pink"))))
 '(hi-red-b ((((min-colors 88)) (:foreground "brown" :weight bold))))
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "gray20"))))
 '(hl-line ((t (:background "gray10"))))
 '(ido-first-match ((t nil)))
 '(ido-subdir ((((min-colors 88) (class color)) (:foreground "bisque"))))
 '(ioccur-match-face ((t (:background "Blue"))))
 '(isearch ((((class color) (min-colors 88) (background dark)) (:background "purple" :foreground "wheat" :weight bold))))
 '(isearch-fail ((((class color) (min-colors 88) (background dark)) (:foreground "red" :weight bold))))
 '(js2-builtin-face ((t (:foreground "wheat3"))))
 '(js2-comment-face ((t (:foreground "PaleGreen4"))))
 '(js2-constant-face ((t (:foreground "wheat3"))))
 '(js2-function-name-face ((t (:foreground "LightYellow"))))
 '(js2-function-param-face ((t nil)))
 '(js2-keyword-face ((t (:underline t))))
 '(js2-string-face ((t (:foreground "wheat4"))))
 '(js2-variable-name-face ((t nil)))
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:inverse-video t))))
 '(link ((((class color) (min-colors 88) (background dark)) (:foreground "lightblue" :underline t))))
 '(markerpen-face-a ((t (:underline "yellow"))))
 '(markerpen-face-b ((t (:foreground "PeachPuff"))))
 '(markerpen-face-c ((t (:background "navy"))))
 '(markerpen-face-f ((t (:foreground "LawnGreen"))))
 '(match ((((class color) (min-colors 88) (background dark)) (:foreground "purple" :weight bold))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "#001515"))))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background dark)) (:background "#003400"))))
 '(org-agenda-date ((t (:inherit org-agenda-structure :foreground "gold" :weight bold))) t)
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "gold" :weight bold))) t)
 '(org-agenda-structure ((((class color) (min-colors 88) (background dark)) (:foreground "linen" :weight bold :height 1.4))))
 '(org-archived ((((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey20"))))
 '(org-block ((t (:family "Monospace"))))
 '(org-column ((t (:background "black" :family "Courier New"))))
 '(org-date ((t (:foreground "LightSlateGray" :height 0.5))))
 '(org-done ((t (:foreground "DarkSlateGrey"))))
 '(org-level-1 ((((class color) (min-colors 88) (background dark)) (:foreground "gold" :height 1.2))))
 '(org-level-2 ((((class color) (min-colors 16) (background dark)) (:foreground "YellowGreen"))))
 '(org-level-3 ((((class color) (min-colors 88) (background dark)) (:foreground "bisque"))))
 '(org-level-4 ((((class color) (min-colors 88) (background dark)) (:foreground "thistle"))))
 '(org-link ((((class color) (background dark)) (:foreground "LavenderBlush3" :underline t))))
 '(org-scheduled ((((class color) (min-colors 88) (background dark)) (:foreground "bisque"))))
 '(org-scheduled-previously ((((class color) (min-colors 88) (background dark)) (:foreground "bisque"))))
 '(org-scheduled-today ((((class color) (min-colors 88) (background dark)) (:inherit org-scheduled))))
 '(org-special-keyword ((t (:foreground "linen" :height 0.4))))
 '(org-table ((((class color) (min-colors 88) (background dark)) (:foreground "wheat"))))
 '(org-tag ((t (:underline t))))
 '(org-time-grid ((((class color) (min-colors 16) (background dark)) nil)))
 '(org-todo ((t (:foreground "Pink" :weight bold :height 0.4))))
 '(org-upcoming-deadline ((((class color) (min-colors 88) (background dark)) (:foreground "salmon4"))))
 '(org-warning ((t (:foreground "salmon1" :weight bold))))
 '(p4-depot-added-face ((t (:foreground "OliveDrab"))) t)
 '(p4-depot-branch-op-face ((t (:foreground "gold"))) t)
 '(p4-depot-deleted-face ((t (:foreground "brown"))) t)
 '(p4-diff-del-face ((t (:foreground "gray" :strike-through t))) t)
 '(p4-diff-ins-face ((t (:foreground "gray"))) t)
 '(paren-face-match ((((class color)) (:background "grey15"))))
 '(py-builtins-face ((t nil)) t)
 '(py-pseudo-keyword-face ((t (:inherit Font\ Lock\ Keyword\ Face))) t)
 '(region ((((class color) (min-colors 88) (background dark)) (:background "gray20"))))
 '(secondary-selection ((((class color) (min-colors 88) (background dark)) (:background "navy"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey30"))))
 '(sqlplus-table-even-rows-face ((((background dark)) (:inherit default :background "#32c832c832c8" :foreground "bisque" :overline "black"))))
 '(sqlplus-table-odd-rows-face ((((background dark)) (:inherit default :background "#4c2c4c2c4c2c" :foreground "bisque" :overline "black"))))
 '(sr-active-path-face ((t (:foreground "yellow" :height 120))))
 '(sr-highlight-path-face ((t (:foreground "#ace6ac" :weight bold))))
 '(sr-passive-path-face ((t (:weight bold))))
 '(vc-annotate-face-3F3FFF ((t (:background "black" :foreground "khaki"))) t)
 '(yas/field-highlight-face ((t (:inverse-video t)))))


(op:log "loading org-custom.el")
(load-file (expand-file-name "~/emacs/org-custom.el")) ;; call after custom-set-variables because some variables (org-agenda-files) must be redefined
(op:log "loading dired-custom.el")
(load-file (expand-file-name "~/emacs/dired-custom.el")) ;; depends on idtv.el


(put 'dired-find-alternate-file 'disabled nil)
