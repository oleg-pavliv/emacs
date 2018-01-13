(require 'cl)
(require 'time-stamp)

(load-file (expand-file-name "~/emacs/misc.el"))
(require 'misc)

(load-file (expand-file-name "~/emacs/nagra-specific.el"))


(set-register ?a "grunt serve:predist")
(set-register ?i "mvn install -Dmaven.test.skip=true")
(set-register ?s "java -Dserver.port=8181 -agentlib:jdwp=transport=dt_socket,server=y,address=6006,suspend=n -jar hello-0.1.0.jar")
(set-register ?j "java -Dserver.port=8181 -Dcom.sun.management.jmxremote -Dcom.sun.management.jmxremote.port=12345 -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false -jar hello-1.0-SNAPSHOT.jar")

(set-register ?1 "find . -type d -name target -exec rm -r {} ';'")
(set-register ?d (cons 'file (or (getenv "DOWNLOADS_DIR") "c:/Users/opavliv/Downloads/")))
(set-register ?k (cons 'file (or (getenv "DESKTOP_DIR") "C:/Users/opavliv/Desktop/")))


(defun op:shell-in-dir(dir buf)
  (when (not (get-buffer buf))
    (dired dir)
    (rename-buffer "temp-buf-for-open-shell")
    (shell buf)
    (kill-buffer "temp-buf-for-open-shell")
    (delete-other-windows)))



(defun op:decompile-java-class (file &optional buf)
  (let ((buf-name (or buf (time-stamp-string))))
    (shell-command (concat (getenv "utils") "/jad.exe -o -p \"" file "\"") buf-name)
    (with-current-buffer buf-name
      (goto-char (point-min))
      (while (re-search-forward "" nil t) (replace-match "" nil t))
      (goto-char (point-min))
      (while (re-search-forward "^JavaClassFileReadException: can't open input file.*?$" nil t) (replace-match "" nil t))  ;; the decompiler shows this message if an inner class has been deleted
      (buffer-substring-no-properties (point-min) (point-max)))))


(defun op:get-imports-exports (file buf-name)
  (shell-command (concat (getenv "utils") "/dumpbin.exe /exports /imports \"" file "\"") buf-name)
  (with-current-buffer buf-name)
  (buffer-substring-no-properties (point-min) (point-max)))


(defun op:look-for-file-in-all-buffers (file line curr-buf)
  (let ((new-buf))
    (save-excursion
      (loop for buf in (buffer-list) until new-buf do
            (when (string-match (concat "^" file) (buffer-name buf))
              (setq new-buf buf)))
      (loop for buf in (buffer-list) until new-buf do
            (when (equal (op:buffer-mode buf) 'dired-mode)
              (switch-to-buffer buf)
              (goto-char (point-min))
              (when (re-search-forward (concat " " file) nil t)
                (dired-find-file)
                (setq new-buf (current-buffer))))))
    (switch-to-buffer (or new-buf curr-buf))
    (if new-buf (goto-line line))))


(defun op:goto-line-in-file (file line)
  (unless (file-exists-p file)
    (setq file (concat "C:/" file)))
  (unless (file-exists-p file)
    (setq file nil))
  (when file
    (find-file file)
    (goto-line line)))


(defun op:goto-line-at-point()
  (let* ((curr-line (op:curr-line)) (curr-buf (current-buffer)))
    (cond
     ((string-match "[0-9][0-9][0-9][0-9]-[0-9 -:]+\"\\([^\"]*\\)" curr-line) ;; 2011-04-19 "./path/to/file" as in find
      (find-file-at-point (match-string 1 curr-line)))
     ((or (string-match "\\([A-Za-z]:[/|\\][^:;]*\\):\\([0-9]+\\)?" curr-line) ;; C:/dir/file.java:102 or D:\dir\file.java:102 string number is optional
          (string-match "^[^\\]*\\([^:]*\\):\\[\\([0-9]+\\)" curr-line)) ;; \Oleg\Dropbox\work\cxf\file-ws\src\main\java\org\op\filews\FileSystemImpl.java:[12,27] 
      (op:goto-line-in-file (match-string 1 curr-line) (string-to-number (or (match-string 2 curr-line) "0"))))
     (t (find-file-at-point)))))


(defun filename-at-point-win ()
  (let* ((from (point-min))
         (to (point-max)))
    (save-excursion 
      (re-search-forward "\\=[ -~/[:alnum:]_.\\]*" nil t) ;;  '\\=' matches empty string but only at point (not clear why it is here), then match every char a filename can contain
      (backward-char)
      (setq to (point))
      (when (re-search-backward "[:\"]" nil t)   ;; search for ':' or for '"'
        (if (equal ":" (match-string 0))
            (backward-char) 
          (forward-char))
        (setq from (point))))
    (buffer-substring-no-properties from to)
    ))


(defun op-i:ffap (arg)
  (interactive "P")
  (if arg
      (if (eq 4 (car arg))
          (ffap)
        (op:w32-shell-open (filename-at-point-win)))
    (op:goto-line-at-point)))

(define-key global-map [C-f1] 'op-i:ffap)

(add-hook 'shell-mode-hook (lambda ()
                             (highlight-lines-matching-regexp "^.*\\(?:xception\\).*$" 'hi-red-b)
                             (highlight-lines-matching-regexp "^.*\\(?: ERROR \\).*$" 'hi-red-b)
                             (highlight-lines-matching-regexp "^.*\\(?: ERROR: \\).*$" 'hi-red-b)
                             (highlight-lines-matching-regexp "^.*\\(?: FATAL \\).*$" 'hi-red-b)))

(setq *op:cmd-frames* '())

(defun op:start-cmd-in-new-frame (dir cmd &optional no-start)
  (let* ((cmd-buf (concat cmd "-shell"))
         (frame-tuple (assoc cmd *op:cmd-frames*))
         (frame (if frame-tuple (cadr frame-tuple))))
    (if frame (delete-frame frame))
    (switch-to-buffer cmd-buf)
    (kill-buffer-quitly t)
    (unless no-start
      (op:shell-in-dir dir cmd-buf)
      (switch-to-buffer cmd-buf)
      (buffer-disable-undo cmd-buf)
      (comint-send-string (get-buffer-process cmd-buf) (concat "./" cmd "\n"))
      (push (list cmd (make-frame)) *op:cmd-frames*)
      (other-frame 1)
      (bubble-buffer-next))))


(defun op:h2 ()
  (interactive)
  (op:shell-in-dir "c:/Soft/H2/bin/" "h2-bin")
  (switch-to-buffer "h2-bin")
  (comint-send-string (get-buffer-process "h2-bin") "./h2w.bat\n")
  (run-at-time "3 sec" nil (lambda () (kill-buffer-quitly t)))
  )


(defun op:mongodb ()
  (interactive)
  (let ((mongo-data-dir default-directory))
    (if (not (file-directory-p (concat mongo-data-dir "journal")))
        (message "Not a mongo data directory !")
      (message "Starting mongo")
      (op:shell-in-dir "c:/soft/mongodb-win32-x86_64-2008plus-ssl-2.5.0/bin/" "mongodb-bin")
      (switch-to-buffer "mongodb-bin")
      (comint-send-string (get-buffer-process "mongodb-bin") (concat  "./mongod.exe --setParameter textSearchEnabled=true  --dbpath " mongo-data-dir "\n"))
      (op:shell-in-dir "c:/soft/mongodb-win32-x86_64-2008plus-ssl-2.5.0/bin/" "mongo-shell")
      (switch-to-buffer "mongo-shell")
      (comint-send-string (get-buffer-process "mongo-shell") "./mongo\n")
      )
    ))


(defun op:tomcat (&optional arg)
  (interactive "P")
  (let ((old-current-dir default-directory)
        (tomcat-dir "c:/Soft/Java/apache-tomcat-8.0.32"))
    (save-excursion ;; save-excursion is necessary to restore the default-directory. Otherwise it will be restored in another buffer

      (setq default-directory (concat tomcat-dir "/bin"))
      (op:send-cmd-to-shell (concat tomcat-dir "/bin/shutdown.bat\n") "tomcat-shutdown")
      (run-at-time "3 sec" nil (lambda () (kill-buffer-quitly t)))
      (unless arg
        (delete-directory (concat tomcat-dir "/work/") t)
        (make-directory (concat tomcat-dir "/work/"))
        (delete-directory (concat tomcat-dir "/logs/") t)
        (make-directory (concat tomcat-dir "/logs/") t)
        (op:send-cmd-to-shell (concat tomcat-dir "/bin/catalina.bat jpda start\n") "tomcat-startup")
        (run-at-time "3 sec" nil (lambda () (kill-buffer-quitly t)))))
    (setq default-directory old-current-dir)))



(provide 'nagra)
