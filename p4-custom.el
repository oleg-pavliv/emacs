(require 'p4)
(require 'cl)

(require 'misc)
(require 'time-stamp)


(defun op:p4-delete-marked ()
  "Call p4-delete for all marked files"
  (interactive)
  (let* ((files (dired-get-marked-files nil nil)))
    (when (dired-mark-pop-up
           "*p4-delete*" nil files dired-deletion-confirmer
           (format "p4-delete %s " (dired-mark-prompt nil files)))
      (p4-noinput-buffer-action "delete" nil nil files)
      (revert-buffer nil t)))) ;; refresh


(defun op:p4-sync-all ()
  (interactive)
  (let* ((old-current-dir default-directory)
         (sync-result)
         (buf-result (get-buffer-create "p4-sync-all-results"))
         )
    (save-excursion ;; save-excursion is necessary to restore the default-directory. Otherwise it will be restored in another buffer
      (with-current-buffer buf-result
        (delete-region (point-min) (point-max)))
      (get-buffer-create "p4-sync-temp-buf")
      (mapc (lambda (dir) 
              (setq default-directory dir)
              (message (concat "synchronizing " dir))
              (kill-buffer "p4-sync-temp-buf")
              (call-process "p4" nil "p4-sync-temp-buf" nil "sync")
              (with-current-buffer "p4-sync-temp-buf"
                (setq sync-result (concat dir "\n" (buffer-substring-no-properties (point-min) (point-max)) "\n")))
              (with-current-buffer buf-result
                (goto-char (point-max))
                (insert sync-result)))
            (mapcar (lambda (p2d) (cdr p2d)) *j2ep-dir-mapping*))
      (switch-to-buffer buf-result)
      (highlight-regexp "^Can't.*" "hi-red-b") ;; e.g Can't clobber a writable file
      (highlight-regexp ".*? deleted .*" "hi-pink")
      (highlight-regexp ".*? must resolve .*" "hi-pink")
      (highlight-regexp ".*? added .*" "hi-green"))
    (setq default-directory old-current-dir)
    ))




;; (defp4cmd p4-revert (show-output)
;;   "revert" "To revert all change in the current file, type \\[p4-revert].\n"
;;   (interactive (list p4-verbose))
;;   (let ((args (p4-buffer-file-name))
;;         refresh-after)
;;     (if (or current-prefix-arg (not args))
;;         (progn
;;           (setq args (if (p4-buffer-file-name-2)
;;                          (p4-buffer-file-name-2)
;;                        ""))
;;           (setq args (p4-make-list-from-string
;;                       (p4-read-arg-string "p4 revert: " args)))
;;           (setq refresh-after t))
;;       (setq args (list args)))
;;     (if (yes-or-no-p "Really revert changes? ")
;;         (let ((filename (p4-buffer-file-name-2)))  ;; we backup a file before performing the revert
;;           (when filename
;;             (copy-file filename (concat filename ".bak") 1 t)
;;             (p4-noinput-buffer-action "revert" t (and show-output 's) args)
;;             (if refresh-after
;;                 (progn
;;                   (p4-refresh-files-in-buffers)
;;                   (p4-check-mode-all-buffers))
;;               (p4-check-mode))
;;             (p4-update-opened-list))))))


(defun invoke-p4v-cmd (cmd)
  (let ((file (if (equal major-mode 'dired-mode)
                  (dired-get-file-for-visit)
                (buffer-file-name)))
        (tmp-buf (time-stamp-string)))
    (when file
      (op:shell-in-dir (op:win-file-perforce-client-dir file) tmp-buf)
      (op:send-cmd-to-shell (concat "p4v.exe -cmd \"" cmd " " (op:win-file-to-perforce file) "\" \n") tmp-buf)
      (run-at-time "5 sec" nil (lambda () (kill-buffer-quitly t))))))


(defp4cmd p4-changes ()
  "changes" "To list changes, type \\[p4-changes].\n"
  (interactive)
  (let ((args "-m 200 ..."))
    (when current-prefix-arg
      (setq args (concat "-m 200 " (if (equal major-mode 'dired-mode) (op:win-file-to-perforce (dired-current-directory))) "...")) ;; original was only "-m 200"
      (setq args (p4-read-arg-string "p4 changes: " args)))
    (p4-file-change-log "changes" (p4-make-list-from-string args))))


(defadvice p4-changes (after p4-changes-other-wnd activate)
  (other-window 1)
  (delete-other-windows))

(defadvice p4-submit (after p4-submit-other-wnd activate)
  (delete-other-windows))

(defadvice p4-filelog (after p4-filelog-other-wnd activate)
  (other-window 1)
  (delete-other-windows))

(defadvice p4-buffer-commands (after p4-buffer-commands-other-wnd activate)
  (delete-other-windows))


(defadvice p4-opened (after p4-opened-other-wnd activate)
  (other-window 1)
  (delete-other-windows))

(defadvice p4-blame (around p4-blame-max-wnd activate) ;; p4-print-with-rev-history is an alias of p4-blame and defadvice does not work with aliases
  (let ((curr-line (line-number-at-pos)))
    ad-do-it
    (other-window 1)
    (forward-line curr-line)
    (delete-other-windows)))



(defun p4-describe-bindings ()
  "A function to list the key bindings for the p4 prefix map"
  (interactive)
  (save-excursion
    ;; (p4-push-window-config)
    (let ((map (make-sparse-keymap))
          (p4-bindings-buffer "*P4 key bindings*"))
      (get-buffer-create p4-bindings-buffer)
      (kill-buffer p4-bindings-buffer)
      (describe-bindings "\C-xp")
      (set-buffer "*Help*")
      (rename-buffer p4-bindings-buffer)
      (define-key map "q" 'p4-quit-current-buffer)
      (use-local-map map)
      (display-buffer p4-bindings-buffer))))


;; (define-key p4-prefix-map "x" (lambda () (interactive) (message "p4-delete has been disabled")))
(define-key p4-prefix-map "l" (lambda () (interactive) (message "p4-labelsync has been disabled")))
(define-key p4-prefix-map "d" 'p4-ediff2)
(define-key p4-prefix-map "?" 'p4-describe-bindings)



(defun op:p4v-timelapse ()
  "show revision tree"
  (interactive)
  (invoke-p4v-cmd "annotate"))

(define-key p4-prefix-map "T" 'op:p4v-timelapse)


(defun op:p4v-tree ()
  "show revision tree"
  (interactive)
  (invoke-p4v-cmd "tree"))

(define-key p4-prefix-map "g" 'op:p4v-tree)

(defun op:p4v-history ()
  "Show history. Original binding p4-delete was disabled"
  (interactive)
  (invoke-p4v-cmd "history"))

(define-key p4-prefix-map "x" 'op:p4v-history)

(defun op:p4-get ()
  "p4-get called with './...' for a directory"
  (interactive)
  (let ((file (if (equal major-mode 'dired-mode) "./..." (buffer-file-name))) (args))
    (when file
      ;; the following code was taken from p4-get and updated
      ;; I don't know how to call p4-get with arguments
      (if current-prefix-arg
          (setq args (p4-make-list-from-string (p4-read-arg-string "p4 get: "))))
      (p4-noinput-buffer-action "get" nil t (append args (list file)))
      (p4-refresh-files-in-buffers)
      (p4-make-depot-list-buffer
       (concat "*P4 Get: (" (p4-current-client) ") " file "*")))))


(define-key p4-prefix-map "G" 'op:p4-get)


(defun op:p4-files-in-interval ()
  (interactive)
  (when (equal major-mode 'dired-mode)
    (let ((dir (op:win-file-to-perforce (dired-current-directory)))
          (shell-buf (get-buffer-create (concat "shell-" (buffer-name))))
          (week-ago (format-time-string "%Y/%m/%d" (time-add (current-time) (seconds-to-time (- (* 3600 24 7)))))))
      (shell shell-buf)
      (with-current-buffer shell-buf 
        (end-of-buffer)
        (insert (concat "p4 files " dir "...@" week-ago ",@now")))
      (delete-other-windows))))


;; (defp4cmd p4-clients ()
;;   "clients" "To list all clients, type \\[p4-clients].\n"
;;   (interactive)
;;   (p4-noinput-buffer-action "clients" nil t (list "-u" "opavliv")) ;; original was nil for arguments
;;   (p4-make-basic-buffer "*P4 clients*")
;;   (p4-regexp-create-links "*P4 clients*" "^Client \\([^ ]+\\).*\n" 'client)
;;   (other-window 1)            ;; the following two lines has been added
;;   (delete-other-windows))


(defun op:p4-labels (&optional arg)
  ;;  "labels" "To display list of defined labels, type \\[p4-labels].\n"
  (interactive "P")                                                                                     ;; this line is different
  (p4-noinput-buffer-action "labels" nil t (append (if arg (list "-u" "opavliv")) (list "-e" "*REL*"))) ;; this line is different
  (p4-make-basic-buffer "*P4 labels*")
  (p4-regexp-create-links "*P4 labels*" "^Label \\([^ ]+\\).*\n" 'label))

(define-key p4-prefix-map "L" 'op:p4-labels)


(defun op-i:diff-with-prev-version()
  "diff with the previous perforce version. Used in submit and describe buffers."
  (interactive)
  (let* ((curr-line (op:curr-line)) (curr-buf (current-buffer)) (version))
    (cond
     ((or (string-match "*Opened Files:" (buffer-name)) (string-match "*P4 Submit" (buffer-name)))
      (when (string-match ".*?/\\(/LYSIS/.*?\\)[[:blank:]]*#" curr-line)                                ;; ... //LYSIS/CMS/filename #
        (find-file (concat (getenv "PERFORCE_WORKSPACE") (match-string 1 curr-line)))
        (p4-ediff)))
     ((string-match "*P4 describe" (buffer-name))
      (when (string-match ".*?/\\(/LYSIS/.*?\\)#\\([0-9]+\\)" curr-line) ;; ... //LYSIS/CMS/filename#version
        (setq version (match-string 2 curr-line))
        (p4-noinput-buffer-action "get" nil t (list "-f" (concat "/" (match-string 1 curr-line) "#" version)))
        (find-file (concat (getenv "PERFORCE_WORKSPACE") (match-string 1 curr-line)))
        (p4-ediff2 version (number-to-string (1- (string-to-number version))))))
     )))

(define-key p4-prefix-map "-" 'op-i:diff-with-prev-version)
(define-key p4-prefix-map "=" 'p4-ediff)



;;--------- p4v commands ---------------
;; history     File history of the specified file
;; open        Open P4V with the specified file
;; submit      Submit the specified file
;; properties  Properties of the specified file
;; diffdialog  Open a dialog to specify two file revisions to compare
;; prevdiff    Compare the local file against the have revision
;; annotate    Timelapse View of the specified file
;; tree        Revision graph of the specified file
;;--------------------------------------------------


;; Thanks, also, for the -win 0 hint.  It seems to work great.

;;(defadvice p4-get (before))
