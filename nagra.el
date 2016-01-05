(require 'cl)
(require 'time-stamp)

(load-file (expand-file-name "~/emacs/misc.el"))
(require 'misc)

(load-file (expand-file-name "~/emacs/nagra-specific.el"))


(set-register ?a "grunt serve:predist")
(set-register ?b "mvn spring-boot:run")
(set-register ?i "mvn install -Dmaven.test.skip=true")
(set-register ?s "java -Dserver.port=8181 -agentlib:jdwp=transport=dt_socket,server=y,address=6006,suspend=n -jar hello-0.1.0.jar")
(set-register ?j "java -Dserver.port=8181 -Dcom.sun.management.jmxremote -Dcom.sun.management.jmxremote.port=12345 -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false -jar hello-1.0-SNAPSHOT.jar")

(set-register ?1 "find . -type d -name target -exec rm -r {} ';'")
(set-register ?d (cons 'file (or (getenv "DOWNLOADS_DIR") "c:/Users/opavliv/Downloads/")))
(set-register ?k (cons 'file (or (getenv "DESKTOP_DIR") "C:/Users/opavliv/Desktop/")))

;; (set-register ?t '(file . "c:/temp/lysis/"))

;; (defun op:java-version (version)
;;   (interactive "nversion: ")
;;   (when (equal 'shell-mode (op:buffer-mode))
;;     (let ((cmd ""))
;;       (cond
;;        ((equal version 5) (setq cmd "JAVA_HOME=$JAVA5;PATH=$JAVA5/bin:$PATH") )
;;        ((equal version 6) (setq cmd "JAVA_HOME=$JAVA6;PATH=$JAVA6/bin:$PATH") )
;;        ((equal version 7) (setq cmd "JAVA_HOME=$JAVA7;PATH=$JAVA7/bin:$PATH") )
;;        )
;;       (insert (concat cmd "\n"))
;;       (insert "java -version"))))


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


(defun op:perforce-ct-to-win (file)
  (loop for el in *j2ep-dir-mapping* 
        until (string-match (concat (car el) "\\(.*\\)") file) 
        finally (return (concat (cdr el) (match-string 1 file)))))


(defun op:win-file-to-perforce (file)
  (loop for el in *j2ep-dir-mapping* 
        until (string-match (concat (cdr el) "\\(.*\\)") file) 
        finally (return (concat (car el) (match-string 1 file)))))

(defun op:win-file-perforce-client-dir (file)
  (let ((client-dir))
    (loop for el in *j2ep-dir-mapping* 
          until (string-match (concat (cdr el) "\\(.*\\)") file) 
          finally (setq client-dir (cdr el)))
    (if (file-exists-p (concat client-dir "p4config.txt"))
        client-dir
      (file-name-directory (directory-file-name client-dir)) ;; upper directory
      )))

(defun op:goto-line-at-point()
  (let* ((curr-line (op:curr-line)) (curr-buf (current-buffer)))
    (cond
     ((string-match "[0-9][0-9][0-9][0-9]-[0-9 -:]+\"\\([^\"]*\\)" curr-line) ;; 2011-04-19 "./path/to/file" as in find
      (find-file-at-point (match-string 1 curr-line)))
     ((or (string-match "\\([A-Za-z]:[/|\\][^:;]*\\):\\([0-9]+\\)?" curr-line) ;; C:/dir/file.java:102 or D:\dir\file.java:102 string number is optional
          (string-match "^[^\\]*\\([^:]*\\):\\[\\([0-9]+\\)" curr-line)) ;; \Oleg\Dropbox\work\cxf\file-ws\src\main\java\org\op\filews\FileSystemImpl.java:[12,27] 
      (op:goto-line-in-file (match-string 1 curr-line) (string-to-number (or (match-string 2 curr-line) "0"))))
     ((or (and 
           ;; (or (string-match "P4[[:blank:]]+print-revs" (buffer-name)) (string-match "P4[[:blank:]]+Output" (buffer-name))) 
           (string-match "^[[:blank:]]*\\([0-9]+\\)[[:blank:]]+" curr-line)) ;;" 91747 " in P4 print-revs or in P4 Output after calling p4-blame
          (and 
           ;; (string-match "P4 print-revs" (buffer-name))
           (string-match "^\\([0-9]+\\)[[:blank:]]+" curr-line))  ;; p4-blame output buffer; change is in the beginning of the line
          (and 
           ;; (string-match "P4[[:blank:]]" (buffer-name))
           (string-match "Change[[:blank:]]+\\([0-9]+\\)" curr-line))) ;; "Change 90321" as in P4 changes output
      (p4-describe-internal (list "-s" (match-string 1 curr-line)))
      (other-window 1)
      (delete-other-windows))
     ((or (string-match ".*?\\(//CT/.*?\\)#\\([0-9]+\\)" curr-line)   ; ... //CT/JAVA/filename#version edit
          (string-match ".*?\\(//CT/.*?\\)[[:blank:]]+#" curr-line))  ; ... //CT/JAVA/filename # as in P4 submit
      (find-file (op:perforce-ct-to-win (match-string 1 curr-line)))
      (delete-other-windows))
     ((string-match ".*?\\(//HEP/.*\\)#?" curr-line) 
      (let* ((file (replace-regexp-in-string "#" "" (match-string 1 curr-line))) 
             (win-file (concat "c:/temp/" (replace-regexp-in-string "/" "-" file))))
        (shell-command (concat "p4 print " file "> " win-file))
        (find-file win-file)))
     ((or (string-match "[([]\\([_-A-Za-z0-9]+\\.java\\):\\([0-9]+\\)" curr-line);;2008-12-27 ERROR [STDERR] at com.lysis.framework.common.util.xml.XMLUtil.writeXMLFile(XMLUtil.java:348)
          (string-match "^[[:blank:]]*\\([_-A-Za-z0-9]+\\.\\).*?[[:blank:]]+line:[[:blank:]]+\\([0-9]+\\)" curr-line) ;;Ims4xpl3dn4Imp30ExportAlgorithm.getCopy(DtvTemplateOwner) line: 1697	
          (string-match "class=\".*?\\.\\([[:alnum:]]+\\)\"" curr-line) ;; class="com.lysis.idtv3.vod.rules.DtvVodItemRulesLevel1"
          )
      (op:look-for-file-in-all-buffers (match-string 1 curr-line) (string-to-number (or (match-string 2 curr-line) "0")) curr-buf))
     ((string-match "^\\([_-A-Za-z0-9]+\\.tjp\\):\\([0-9]+\\)" curr-line)
      (op:goto-line-in-file (match-string 1 curr-line) (string-to-number (match-string 2 curr-line))))
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


(defun op:pds-tool2-spring-boot (&optional arg)
  (interactive "P")
  (op:start-cmd-in-new-frame "c:/work/pds-tool2/" "c:/Soft/Java/apache-maven-3.3.3/bin/mvn" arg)
  )


;; <plugin>
;;     <groupId>org.springframework.boot</groupId>
;;     <artifactId>spring-boot-maven-plugin</artifactId>
;;     <configuration>
;;         <jvmArguments>
;;             -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=6006 -Dserver.port=8181
;;         </jvmArguments>
;;     </configuration>
;; </plugin>

;; (defun op:eap-jboss (&optional arg)
;;   (interactive "P")
;;   (setenv "HOSTNAME" "helablinux01")
;;   (setenv "SERVICE_NAME" "service_test")
;;   (setenv "J2EP_DEBUG" "c:/temp/j2ep_internal.log")
;;   (op:start-cmd-in-new-frame "c:/work/JBossEAP/jboss_cct_base/current/bin/" "run_dev.bat" arg)
;;   )

(defun op:h2 ()
  (interactive)
  (op:shell-in-dir "c:/Soft/H2/bin/" "h2-bin")
  (switch-to-buffer "h2-bin")
  (comint-send-string (get-buffer-process "h2-bin") "./h2w.bat\n")
  (run-at-time "3 sec" nil (lambda () (kill-buffer-quitly t)))
  )


(defun op:mongodb ()
  (interactive)
  (op:shell-in-dir "c:/Soft/mongodb-win32-x86_64-2008plus-2.4.14/bin/" "mongodb-bin")
  (switch-to-buffer "mongodb-bin")
  (comint-send-string (get-buffer-process "mongodb-bin") "./mongod.exe --setParameter textSearchEnabled=true  --dbpath c:/temp/mongo-data\n" )
  )


;; (defun op:rm-eap-jboss-dir ()
;;   (interactive)
;;   (shell-command "taskkill /F /IM cmd.exe /T")
;;   (delete-directory "c:/work/JBossEAP/jboss_cct_base/" t))


;; (defun op:jmeter ()
;;   (interactive)
;;   (start-process-shell-command "jmeter" nil "ant -f c:/p4_ws/JBossEAP_test/testing/jmeter/scripts/j2ep/test-suites/jmeter-suite-all.xml start-interactive-jmeter" ))

(defun op:tomcat (&optional arg)
  (interactive "P")
  (let ((old-current-dir default-directory))
    (save-excursion ;; save-excursion is necessary to restore the default-directory. Otherwise it will be restored in another buffer

      (setq default-directory "c:/Soft/Java/apache-tomcat-8.0.23/bin")
      (op:send-cmd-to-shell "c:/Soft/Java/apache-tomcat-8.0.23/bin/shutdown.bat\n" "tomcat-shutdown")
      (run-at-time "3 sec" nil (lambda () (kill-buffer-quitly t)))
      (unless arg
        (delete-directory "c:/Soft/Java/apache-tomcat-8.0.23/work/" t)
        (make-directory "c:/Soft/Java/apache-tomcat-8.0.23/work/")
        (delete-directory "c:/Soft/Java/apache-tomcat-8.0.23/logs/" t)
        (make-directory "c:/Soft/Java/apache-tomcat-8.0.23/logs/" t)
        (op:send-cmd-to-shell "c:/Soft/Java/apache-tomcat-8.0.23/bin/catalina.bat jpda start\n" "tomcat-startup")
        (run-at-time "3 sec" nil (lambda () (kill-buffer-quitly t)))))
    (setq default-directory old-current-dir)))


(defun op:camunda ()
  (interactive)
  (let ((old-current-dir default-directory))
    (save-excursion ;; save-excursion is necessary to restore the default-directory. Otherwise it will be restored in another buffer
      (setq default-directory "c:/Soft/camunda/tomcat")
      (op:send-cmd-to-shell "cd c:/Soft/camunda/tomcat\n./start-camunda.bat\n" "camunda-startup")
    (setq default-directory old-current-dir)
    ))
  ;; ((op:w32-shell-open "c:/Soft/camunda/tomcat/start-camunda.bat")
  (run-at-time "3 sec" nil (lambda () (kill-buffer-quitly t)))
  )


(defun op:camunda-eval ()
  (interactive)
  (let ((old-current-dir default-directory))
    (save-excursion ;; save-excursion is necessary to restore the default-directory. Otherwise it will be restored in another buffer
      (setq default-directory "c:/Soft/camunda-eval")
      (op:send-cmd-to-shell "cd c:/Soft/camunda-eval\n./start-camunda.bat\n" "camunda-startup")
    (setq default-directory old-current-dir)
    ))
  ;; ((op:w32-shell-open "c:/Soft/camunda/tomcat/start-camunda.bat")
  (run-at-time "3 sec" nil (lambda () (kill-buffer-quitly t)))
  )




(provide 'nagra)
