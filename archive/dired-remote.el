;;------------------------------------------------- remote operations -----------------------------------------------------------

(setq *op:rs-current-host* nil)
(setq *op:rs-delete-remote-files* nil)
;;(setq *op:rs-local-base-dir* nil)


(defun op:rs-get-current-host (&optional signal-error)
  (or *op:rs-current-host* (and signal-error (error "Call op:rs-set-current-host"))))

(defun op:rs-set-current-host (host base-local-dir user pwd)
  (interactive "sHost: \nDBase dir: \nsUser: \nsPassword: ")
  (if (or (equal host "nil") (equal host "null")) 
      (setq *op:rs-current-host* nil)
    (progn 
      (op:rs-get-process-create host (op:winfile-to-cygfile base-local-dir) (or (op:str-null user) "lysis") (or (op:str-null pwd) "lysis_1"))
      (setq *op:rs-current-host* host)
      (setq *op:rs-delete-remote-files* nil))))


(setq *op:rs-host-map* '())

(defun op:rs-switch-host ()
  (interactive)
  (let ((host (completing-read "Switch host: " (append (mapcar #'car *op:rs-host-map*) (list "nil")) nil t *op:rs-current-host*)))
    (if (equal host "nil")
        (setq *op:rs-current-host* nil)
      (setq *op:rs-current-host* host))))


(defun op:rs-get-process-create (host &optional base-local-dir user pwd)
  "return (process . output-buffer) associated wiht host. Create if necessary"
  (unless (assoc host *op:rs-host-map*) 
    (let* ((buffer (concat "*lftp-output-" host))
           (process (start-process "lftp" (get-buffer-create buffer) "lftp" (concat user ":" pwd "@" host))))
      (push (cons host (list process buffer base-local-dir)) *op:rs-host-map*)
      (op:mark-buffer-as-permanent buffer)
      (op:rs-send-cmd-async host "set -a cache:enable false")))
  (cdr (assoc host *op:rs-host-map*)))


(defun op:rs-get-local-base-dir (host)
  (let ((pbd (assoc host *op:rs-host-map*)))
    (and pbd (cadddr pbd))))


(defmacro op:rs-send-cmd-get-result (host cmd wait &rest form)
  "send cmd to the process asscociated with a host. execute result-form"
  `(let* ((pbd (op:rs-get-process-create ,host)) (process (car pbd)) (out-buf (cadr pbd)))
     (with-current-buffer out-buf
       (kill-region (point-min) (point-max))
       (process-send-string process (concat ,cmd "\n"))
       (if ,wait (accept-process-output process)) ;; wait for output
       (goto-char (point-min))
       ,@form)))

(defun op:rs-send-cmd-async (host cmd)
  (op:rs-send-cmd-get-result host cmd nil))

(defmacro op:rs-send-cmd (host cmd &rest form)
  `(op:rs-send-cmd-get-result ,host ,cmd t ,@form))


;; (defun op:rs-get-remote-jboss-dir (host)
;;   (op:rs-send-cmd host "find -d 2 /soft/idtvsrv/ | grep jboss.*/$"
;;                       (move-end-of-line 1)
;;                       (buffer-substring-no-properties (point-min) (point))))


(defun op:rs-cls (host dir opts)
  (op:rs-send-cmd host (concat "cls " opts " " dir)
                  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))


;; (defun op:rs-cmd-sync-remote-dirs (host remote local)
;;   (shell-command (format "lftp -c 'open -e \"mirror -X '*.*' %s %s\" lysis:lysis_1@%s'" remote local host)))

(defun op:rs-cmd-mirror-remote-dirs (host from-dir to-dir &optional opts include-glob exclude-glob newer-than)
  (op:rs-send-cmd-async host (format "mirror %s %s %s %s %s %s"
                                     (or opts "")
                                     (if (op:str-null include-glob) (concat "-I '" include-glob "'") "")
                                     (if (op:str-null exclude-glob) (concat "-X '" exclude-glob "'") "")
                                     (if (> (length newer-than) 0) (concat "--newer-than=" newer-than) "")
                                     from-dir to-dir)))

(defun op:rs-cmd-mget (host local-dir remote-dir files)
  (make-directory local-dir t)
  (op:rs-send-cmd-async host (concat "mget -O " (op:winfile-to-cygfile local-dir) " " remote-dir files)))


(defun op:rs-dir-mode (host mode remote-dir)
  "set mode to remote-dir, creating if necessary and optionally all its subdirs"
  (let* ((cd "/") (rmd (op:str-to-remote-dir remote-file)) (rd-ls) (dir-exist))
    (if (equal "/" (substring rmd 0 1)) (setq rmd (substring rmd 1)))
    (if (equal "/" (substring rmd (- (length rmd) 1))) (setq rmd (substring rmd 0 (- (length rmd) 1))))
    (setq rd-ls (split-string rmd "/"))
    (dolist (rd rd-ls)
      (setq dir-exist (member (concat rd "/") (op:rs-cls host cd "-1B")))
      (setq cd (concat cd rd "/"))
      (unless dir-exist
        (op:rs-send-cmd-async host (concat "mkdir -p " cd))
        (op:rs-send-cmd-async host (concat "chmod " mode " " cd))))))

;; TODO get mode of existing file and chmod 
(defun op:rs-cmd-put (host local-file remote-file)
  (op:rs-dir-mode host "777" (op:str-to-remote-dir remote-file))
  (op:rs-send-cmd-async host (concat "put " (op:winfile-to-cygfile local-file) " -o " remote-file))
  (op:rs-send-cmd-async host (concat "chmod " (format " %o " (file-modes local-file)) remote-file)))


(defun op:rs-cmd-rm (host remote-file &optional recursive)
  (op:rs-send-cmd-async host (concat "rm " (and recursive "-r ") remote-file)))


(defun op:str-to-remote-dir (str)
  "converts 'foo/bar/file.txt' to '/foo/bar/'"
  (let ((remote (if (equal "/" (substring str 0 1)) str (concat "/" str))))
    (string-match "\\(.*/\\)[^/]*" remote)
    (match-string-no-properties 1 remote)))


(defun op:rs-remote-complete (remote-dir predicate-host flag)
  "predicate-host is a host and not a predicate. See a comment in op:prompt-remote-file"
  (let* ((rd (op:str-to-remote-dir remote-dir))
         (host predicate-host)
         (files (mapcar (lambda (x) (concat rd x)) (op:rs-cls host rd "-1B"))))
    (cond
     ((null flag) 
      (try-completion remote-dir files))
     ((equal t flag) 
      (all-completions remote-dir files))
     ((and (symbolp flag) (equal "lambda" (symbol-name flag)))
      (test-completion remote-dir files)))))


(defun op:prompt-remote-file (host prompt &optional require-match)
  (partial-completion-mode -1) ;; otherwise it does not handle hyphens correctly in PC-do-completion
  (unwind-protect
      ;; !!! HACK CODE: host passed as a predicate parameter because there is no other way to pass a parameter to a op:rs-remote-complete
      (completing-read prompt 'op:rs-remote-complete host require-match (op:rs-local-to-remote host))
    (partial-completion-mode 1)))


(defun op:rs-is-remote-mirror (base-dir local-file)
  (let ((len (length base-dir)))
    ;; converting to downcase becase base-dir case might be incorrect. it is read from the command line and might be entered in downcase
    (and (op:rs-get-current-host) local-file (>= (length local-file) len) (equal (downcase base-dir) (downcase (substring local-file 0 len))))))


(defun op:rs-local-to-remote (host &optional local-file)
  (let ((lf (op:winfile-to-cygfile (or local-file (dired-current-directory))))
        (base-dir (op:rs-get-local-base-dir (or host (op:rs-get-current-host t)))))
    (if (op:rs-is-remote-mirror base-dir lf)
        (substring lf (- (length base-dir) 1))
      "/")))


(defun op:rs-remote-to-local (host remote-file)
  (concat (op:rs-get-local-base-dir host) remote-file))


(defun op:delayed-refresh-dir (dir &optional sec)
  (run-at-time (concat (prin1-to-string (or sec 1)) " sec") nil #'dired dir "-lhIA"))


(setq *op:rs-mirror-timer* nil)

(defun op:rs-mirror (&optional arg)
  "mirror remote directory. if arg it prompts for include and exclude files and newer-than. repeat format is 'now + count time-units'"
  (interactive "P")
  (op:rs-cancel-mirror)  
  (let* ((host (op:rs-get-current-host t)) 
         (remote-dir (op:prompt-remote-file host "mirror dir: " t))
         (local-dir (op:rs-remote-to-local host remote-dir))
         (newer-than (if arg (read-string "newer than (minutes/hours/days): " "now-1minutes" nil "now-1minutes")))
         (include (if arg (read-string "include files: "))) 
         (exclude (if arg (read-string "exclude files: "))))
    (if (and (numberp arg) (> arg 0)) 
        (setq *op:rs-mirror-timer* (run-at-time nil arg (lambda ()
                                                          (message (concat "mirroring " remote-dir))
                                                          (op:rs-cmd-mirror-remote-dirs host remote-dir local-dir "--delete" include exclude newer-than))))
      (op:rs-cmd-mirror-remote-dirs host remote-dir local-dir "--delete" include exclude newer-than))
    (op:delayed-refresh-dir local-dir)))


(defun op:rs-cancel-mirror ()
  (interactive)
  (if *op:rs-mirror-timer* (cancel-timer *op:rs-mirror-timer*))
  (setq *op:rs-mirror-timer* nil))


(defun op:rs-mget ()
  (let* ((host (op:rs-get-current-host t)) (input (op:prompt-remote-file host "mget files: ")) (regexp "\\(.*?/\\)\\([^/]*\\)$") 
         (remote-dir (replace-regexp-in-string regexp "\\1" input)) 
         (files (replace-regexp-in-string regexp "\\2" input))
         (local-dir (op:rs-remote-to-local host remote-dir)))
    (if (equal files "") (setq files "*"))
    (op:rs-cmd-mget host local-dir remote-dir files)
    (op:delayed-refresh-dir local-dir)))


(define-key dired-mode-map (kbd "C-c g")  (lambda (&optional min)
                                            "mirror remote dir: get remote files locally"
                                            (interactive "P")
                                            (let* ((host (op:rs-get-current-host t))
                                                   (remote-dir (op:rs-local-to-remote host))
                                                   (local-dir (op:rs-remote-to-local host remote-dir)))
                                              (when (not (equal "/" remote-dir))
                                                (message (concat "updating remote dir" remote-dir))
                                                (op:rs-cmd-mirror-remote-dirs host remote-dir local-dir "--delete" nil nil (format "now-%sminutes" (or min "1440")))
                                                (op:delayed-refresh-dir local-dir)))))


(define-key dired-mode-map (kbd "C-c C-f") (lambda () "op:rs-mget mget remote files" (interactive) (op:rs-mget)))


(defadvice save-buffer (after save-buffer-and-put activate)
  (let* ((local-file (op:winfile-to-cygfile (buffer-file-name)))
         (host (op:rs-get-current-host))
         (base-dir (op:rs-get-local-base-dir host)))
    (when (op:rs-is-remote-mirror base-dir local-file)
      (op:rs-cmd-put host local-file (op:rs-local-to-remote host local-file))
      (message (concat "file " local-file " has been put on the remote host")))))


(define-key dired-mode-map (kbd "C-c p") (lambda ()
                                           "put marked files on remote dir"
                                           (interactive)
                                           (let* ((host (op:rs-get-current-host t))
                                                  (local-file (op:winfile-to-cygfile (dired-get-filename)))
                                                  (remote-dir (op:prompt-remote-file host "put file(s) into dir: " t)))
                                             (dolist (local-file (dired-get-marked-files))
                                               (let* ((file-name (file-name-nondirectory local-file))
                                                      (remote-file (concat remote-dir file-name)))
                                                 (if (file-directory-p local-file)
                                                     (op:rs-cmd-mirror-remote-dirs host local-file remote-file " -R ")
                                                   (op:rs-cmd-put host local-file remote-file))
                                                 (message (concat "put " file-name " on " host)))))))


(defun op:rs-chmod (mode)
  (interactive "sMode: ")
  (let* ((host (op:rs-get-current-host t)) (rd (op:prompt-remote-file host "chmod dir: " t)))
    (op:rs-send-cmd-async host (concat "chmod -R " mode " " rd))))


(define-key dired-mode-map (kbd "C-c D") (lambda (arg)
                                           "same as dired-do-delete but remotely"
                                           (interactive "P")
                                           (op:rs-get-current-host t)
                                           (setq *op:rs-delete-remote-files* t)
                                           (unwind-protect
                                               (dired-do-delete arg)
                                             (setq *op:rs-delete-remote-files* nil))))


(define-key dired-mode-map (kbd "C-c x") (lambda ()
                                           "same as dired-do-flagged-delete but remotely"
                                           (interactive)
                                           (op:rs-get-current-host t)
                                           (setq *op:rs-delete-remote-files* t)
                                           (unwind-protect
                                               (dired-do-flagged-delete)
                                             (setq *op:rs-delete-remote-files* nil))))


(defadvice dired-delete-file (after dired-delete-file-and-rm activate)
  (let* ((host (op:rs-get-current-host))
         (base-dir (op:rs-get-local-base-dir host)))
    (when (and *op:rs-delete-remote-files* (op:rs-is-remote-mirror base-dir (op:winfile-to-cygfile file)))
      (op:rs-cmd-rm host (op:rs-local-to-remote host file) t))))


