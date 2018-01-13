(require 'cl)

(setq *jb-dir* "d:/Work/jboss-4.2.2.GA/")

(defun op:str-cut(str len) 
  (substring str 0 (min len (length str))))

(defun op:replace-string(str from to)
  (let ((result str))
    (while (string-match from result)
      (setq result (replace-match to t t result)))
    result))


(defun op:set-twiddle-classpath()
  (setenv "JBOSS_CLASSPATH" (concat *jb-dir* "client/jbossall-client.jar;" *jb-dir* "client/getopt.jar;" *jb-dir* "client/log4j.jar;" *jb-dir* "lib/jboss-jmx.jar;" *jb-dir* "lib/xml-apis.jar;" *jb-dir* "lib/xercesImpl.jar;" *jb-dir* "lib/dom4j.jar;" *jb-dir* "server/default/lib/jboss-management.jar")))


(defun op:jboss-bean-stats(server)
  (interactive (list (read-string (concat "server (localhost):"))))
  (if (equal 0 (length server)) (setq server "localhost"))
  (op:set-twiddle-classpath)
  (message (concat "fetching server beans for the server " server))
  (let ((beans) (user "root") (pwd "management") (stats) (funcs) (match-end 0) 
         (query-beans (shell-command-to-string (concat *jb-dir* "bin/twiddle.bat --server=\"" server "\" query \"*:*\""))))
    (while (string-match "jboss.management.local\:name=.*?\\([a-zA-Z0-9_-]+\\),.*$" query-beans match-end)
      ;;     (while (string-match "jboss.management.local\:name=.*?\\(VODManagementCore\\),.*$" query-beans match-end)
      (push (cons (match-string 0 query-beans) (match-string 1 query-beans)) beans)
      (setq match-end (match-end 0)))
    (dolist (bean beans)
      (message (concat "getting bean stats " (cdr bean)))
      (setq match-end 0 stats (shell-command-to-string (concat *jb-dir* "bin/twiddle.bat --server=\"" server "\" --user=\"" user "\" --password=\"" pwd "\" get \"" (car bean) "\" stats")))
      (while (string-match ".*?\\([a-zA-Z0-9_-]*\\)=\\[.*?Count\:[[:blank:]]*\\([0-9]+\\).*?Total Time\:[[:blank:]]*\\([0-9]+\\).*?\\]" stats match-end)
        (push (list (cdr bean) (match-string 1 stats) (string-to-number (match-string 2 stats)) (string-to-number (match-string 3 stats))) funcs)
        (setq match-end (match-end 0))))
    (setq funcs (sort funcs (lambda (f1 f2) (> (cadddr f1) (cadddr f2)))))
    (message "--------------------------------------------------------------------------------------------")
    (message (concat (format "%-30s" "Bean") (format "%-30s" "Function") (format "%7s" "Count") (format "%15s" "Total time") (format "%10s" "Avg")))
    (message "--------------------------------------------------------------------------------------------")
    (loop for f in funcs for i from 0 to 10 do 
          (message (concat 
                    (format "%-30s" (op:str-cut (car f) 28)) 
                    (format "%-30s" (op:str-cut (cadr f) 28)) 
                    (format "%7d" (caddr f)) 
                    (format "%15.3f" (/ (float (cadddr f)) 1000)) 
                    (format "%10.3f" (/ (/ (float (cadddr f)) (caddr f)) 1000)))))))


(defun op:dump-threads(server)
  (interactive (list (read-string (concat "server (localhost):"))))
  (op:set-twiddle-classpath)
  (let ((match-end 0) (thread) (lysis-threads) (all-threads (shell-command-to-string (concat *jb-dir* "bin/twiddle.bat --server=\"" server "\" invoke \"jboss.system:type=ServerInfo\"  listThreadDump"))))
    (while (string-match "\\(Thread: .*?\\)Thread: " all-threads match-end)
      (setq match-end (match-end 1) thread (match-string 1 all-threads))
      (if (string-match ".*?lysis.*?" thread)
          (setq lysis-threads (concat lysis-threads (op:replace-string (op:replace-string (op:replace-string thread "<br>" "\n") "<.*?>" "") "Thread: " "\nThread : ")))))
    (message lysis-threads)))

(defun op:toplink-cache-stats(server)
  (interactive (list (read-string (concat "server (localhost):"))))
  (op:set-twiddle-classpath)
  (let ((match-end 0) (objects)
        (stats (shell-command-to-string (concat *jb-dir* "bin/twiddle.bat --server=\"" server "\" invoke \"idtv3:service=HealthCheck\"  displayCacheStatistics"))))
    (while (string-match "\\.\\([a-zA-Z0-9_-]+\\)[[:space:]]*:[[:space:]]+\\([0-9]+\\)" stats match-end)
      (push (list (match-string 1 stats) (string-to-number (match-string 2 stats))) objects)
      (setq match-end (match-end 0)))
    (setq objects (sort objects (lambda (o1 o2) (> (cadr o1) (cadr o2)))))
    (message "----------------------------------------")
    (message (concat (format "%-30s" "Object") (format "%10s" "Count")))
    (message "----------------------------------------")
    (loop for o in objects while (>= (cadr o) 1000) do
          (message (concat 
                    (format "%-30s" (op:str-cut (car o) 28))
                    (format "%10d" (cadr o)))))))

(let ((server "localhost") (op))
  (dolist (a command-line-args)
    (when (string-match "server=\\(.*\\)" a) 
      (setq server (match-string 1 a)))
    (when (string-match "operation=\\(.*\\)" a) 
      (setq op (match-string 1 a))))
  (setq op (intern-soft op))
  (unless op (message "usage>emacs.exe -batch -l query-jboss.el -kill server=server-name operation=[op:dump-threads][op:jboss-bean-stats]"))
  (if op (funcall op server)))

;; (op:toplink-cache-stats "10.0.224.20")
;;d:\Soft\Emacs\emacs-23.1\bin\emacs.exe -batch -l query-jboss.el -kill server=10.0.204.81 operation=op:jboss-bean-stats
;;d:\Soft\Emacs\emacs-23.1\bin\emacs.exe -batch -l query-jboss.el -kill server=10.0.204.81 operation=op:toplink-cache-stats
