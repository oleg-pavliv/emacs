(require 'cl)
;; this module contains several utilities to extract archives recursively, compare two archive files and process server.log and health-check files
;;
;; in the code cddr is used to skip "." and ".." directories

(load-file (expand-file-name "~/emacs/idtv.el"))
(require 'idtv)


(defun op:extract(file)
  (message (concat "Extracting file:" file))
  (let ((zip-dir (replace-regexp-in-string "\\.\\([a-zA-Z0-9]*\\)$" "_\\1" file)))
    (make-directory zip-dir)
    (call-process "D:/Utils/7-Zip/7z.exe" nil nil nil  "x" (concat "-o" zip-dir) file)
    zip-dir))

;; the following files won't be extracted, both from op:extract-all-archives and from op:cmp-2dirs
(setq *ignore-files-to-extract* '("activation.jar" "commons-beanutils.jar" "commons-fileupload-1.1.1.jar" "commons-fileupload.jar" "commons-io-1.2.jar" "commons-lang-2.3.jar" "avalon-framework-4.1.5.jar" "barcode4j.jar" "bcpg-jdk15-140.jar" "bcprov-jdk15-139.jar" "bsf.jar" "bsh-core-2.0b4.jar" "castor.jar" "commons-beanutils.jar" "commons-collections.jar" "commons-digester.jar" "commons-httpclient.jar" "commons-logging.jar" "commons-net-1.3.0.jar" "grammatica-bin-1.4.jar" "itext-1.3.jar" "jacorb.jar" "jakarta-oro-2.0.8.jar" "jakarta-poi-1.5.1-final-20020615.jar" "jasperreports.jar" "javax.servlet.jar" "jdom.jar" "jgraph.jar" "jhall.jar" "joesnmp.jar" "jsch-0.1.26.jar" "l2fprod-common-all.jar" "l2fprod-common-buttonbar.jar" "l2fprod-common-directorychooser.jar" "l2fprod-common-fontchooser.jar" "l2fprod-common-outlookbar.jar" "l2fprod-common-sandbox.jar" "l2fprod-common-shared.jar" "l2fprod-common-sheet.jar" "l2fprod-common-springrcp.jar" "l2fprod-common-tasks.jar" "l2fprod-common-totd.jar" "log4j-udp.jar" "logkit-1.2.jar" "mail.jar" "mibble-parser-2.8.jar" "msbase.jar" "mssqlserver.jar" "msutil.jar" "ojdbc14.jar" "saxon9-dom.jar" "saxon9.jar" "toplink.jar" "antlr-2.7.2.jar" "bsf-2.3.0.jar" "commons-beanutils-1.7.0.jar" "commons-chain-1.1.jar" "commons-digester-1.8.jar" "commons-fileupload-1.1.1.jar" "commons-io-1.1.jar" "commons-logging-1.0.4.jar" "commons-validator-1.3.1.jar" "jstl-1.0.2.jar" "oro-2.0.8.jar" "standard-1.0.2.jar" "struts-core-1.3.8.jar" "struts-el-1.3.8.jar" "struts-extras-1.3.8.jar" "struts-faces-1.3.8.jar" "struts-mailreader-dao-1.3.8.jar" "struts-scripting-1.3.8.jar" "struts-taglib-1.3.8.jar" "struts-tiles-1.3.8.jar" "commons-collections-2.0.jar" "commons-logging.jar" "maven-xdoclet-plugin-1.2.2.jar" "xdoclet-1.2.2.jar" "xdoclet-apache-module-1.2.2.jar" "xdoclet-bea-module-1.2.2.jar" "xdoclet-borland-module-1.2.2.jar" "xdoclet-caucho-module-1.2.2.jar" "xdoclet-de-locale-1.2.2.jar" "xdoclet-ejb-module-1.2.2.jar" "xdoclet-exolab-module-1.2.2.jar" "xdoclet-fr_FR-locale-1.2.2.jar" "xdoclet-hibernate-module-1.2.2.jar" "xdoclet-hp-module-1.2.2.jar" "xdoclet-ibm-module-1.2.2.jar" "xdoclet-java-module-1.2.2.jar" "xdoclet-jboss-module-1.2.2.jar" "xdoclet-jdo-module-1.2.2.jar" "xdoclet-jmx-module-1.2.2.jar" "xdoclet-jsf-module-1.2.2.jar" "xdoclet-libelis-module-1.2.2.jar" "xdoclet-macromedia-module-1.2.2.jar" "xdoclet-mockobjects-module-1.2.2.jar" "xdoclet-mvcsoft-module-1.2.2.jar" "xdoclet-mx4j-module-1.2.2.jar" "xdoclet-objectweb-module-1.2.2.jar" "xdoclet-openejb-module-1.2.2.jar" "xdoclet-oracle-module-1.2.2.jar" "xdoclet-orion-module-1.2.2.jar" "xdoclet-portlet-module-1.2.2.jar" "xdoclet-pramati-module-1.2.2.jar" "xdoclet-pt_BR-locale-1.2.2.jar" "xdoclet-solarmetric-module-1.2.2.jar" "xdoclet-spring-module-1.2.2.jar" "xdoclet-sun-module-1.2.2.jar" "xdoclet-sybase-module-1.2.2.jar" "xdoclet-tjdo-module-1.2.2.jar" "xdoclet-web-module-1.2.2.jar" "xdoclet-webwork-module-1.2.2.jar" "xdoclet-wsee-module-1.2.2.jar" "xdoclet-xdoclet-module-1.2.2.jar" "xjavadoc-1.1-j5-v4.jar" "xercesImpl.jar" "xerces_patch.jar" "xml-apis.jar"))


(setq *compare-files-as-text* '("\\.java$" "\\.xml$" "\\.xsl$" "\\.dtd$" "\\.txt$" "\\.html$" "\\.jsp$" "\\.bat$" "\\.sh$" "\\.conf$" "\\.properties$"))


(defun op:is-archive-ext(name)
  (or (string-match "\\.gz$" name) (string-match "\\.gzip$" name) (string-match "\\.tar$" name) (string-match "\\.zip$" name) (string-match "\\.jar$" name) (string-match "\\.ear$" name) (string-match "\\.war$" name) (string-match "\\.sar$" name) (string-match "\\.rar$" name)))


(defun op:find-arch-files(dir)
  (mapcan #'(lambda(attrs) 
              (let* ((full-name (first attrs)) (is-dir (second attrs)) (name (file-name-nondirectory full-name)))
                (or
                 (and is-dir (op:find-arch-files full-name))
                 (and (not is-dir) (op:is-archive-ext full-name) (not (member name *ignore-files-to-extract*)) (list full-name)))))
          (cddr (directory-files-and-attributes dir t)))) 


(defun op:extract-all-archives(dir)
  (loop for arch-lst = (op:find-arch-files dir) for extracted = nil while (or extracted arch-lst) do 
        (dolist (arch arch-lst)
          (setq extracted t)
          (op:extract arch)
          (delete-file arch))))


(defun op:cmp-files-with-diff(file1 file2 size1 size2)
  (if (member (file-name-extension file1) *compare-files-as-text*)
      (call-process "diff" nil "diff-buff" nil  "--strip-trailing-cr" "-w" file1 file2)
    (if (equal size1 size2) 
        (call-process "diff" nil "diff-buff" nil  "--binary" file1 file2)
      1)))

;; (with-current-buffer "diff-buff"
;;     (message (concat "diff:" (buffer-substring-no-properties (point-min) (point-max)) "\n")))

(defun op:string<=(str1 str2)
  (or (equal str1 str2) (string< str1 str2)))


(defun op:delete-identical-files(dir1 dir2 &optional files-not-eq)
  "deletes identical files in dir1 and dir2. files-not-eq is called if files are binary different. if it returns true files are considered equal and are deleted"
  (message (concat "comparing directories " dir1 "," dir2))
  (do* ((inc1 t) (inc2 t) 
        (files1 (cddr (directory-files-and-attributes dir1 t)) (if inc1 (cdr files1) files1)) ; cddr to skip "." and ".." directories
        (files2 (cddr (directory-files-and-attributes dir2 t)) (if inc2 (cdr files2) files2))
        (all-files-eq (equal (null files1) (null files2)))
        (attrs1 (car files1) (car files1)) (attrs2 (car files2) (car files2)))
      ((or (null files1) (null files2)) all-files-eq)
    (let* ((full-name1 (first attrs1)) (is-dir1 (second attrs1)) (size1 (ninth attrs1)) (name1 (file-name-nondirectory full-name1))
           (full-name2 (first attrs2)) (is-dir2 (second attrs2)) (size2 (ninth attrs2)) (name2 (file-name-nondirectory full-name2)) (files-eq nil))
      (when (and (equal name1 name2) (equal is-dir1 is-dir2))
        (when (or (and is-dir1 (op:delete-identical-files full-name1 full-name2 files-not-eq))
;;                  (and (not is-dir1) (equal size1 size2) (= 0 (op:files-binary-equal full-name1 full-name2))) ;; size comparison was removed to be able to do "diff -w"
                  (and (not is-dir1) (= 0 (op:cmp-files-with-diff full-name1 full-name2 size1 size2)))
                  (and (not is-dir1) files-not-eq (funcall files-not-eq full-name1 full-name2)))
          (when (file-exists-p full-name1) (if is-dir1 (delete-directory full-name1) (delete-file full-name1)))
          (when (file-exists-p full-name2) (if is-dir2 (delete-directory full-name2) (delete-file full-name2)))
          (setq files-eq t)))
      (unless files-eq (setq all-files-eq nil))
      (setq inc1 (op:string<= name1 name2))
      (setq inc2 (op:string<= name2 name1)))))


(defun op:equal-files? (file1 file2)
  "compare two binary-different files. if these are archives extract them and continue recursively. 
manifest, nagra.dsa and nagra.sf files are always considered as equal. java class files are decompiled and compared. "
  (cond 
   ((and (op:is-archive-ext file1) (not (member (file-name-nondirectory file1) *ignore-files-to-extract*)))
    (let ((d1 (op:extract file1)) (d2 (op:extract file2)))
      (delete-file file1)
      (delete-file file2)
      (op:delete-identical-and-extract-diff d1 d2)
      (unless (cddr (directory-files-and-attributes d1)) (delete-directory d1)) 
      (unless (cddr (directory-files-and-attributes d2)) (delete-directory d2))))
   ((string-match "manifest\\.mf$" file1) t)
   ((string-match "nagra\\.dsa$" file1) t)
   ((string-match "nagra\\.sf$" file1) t)
   ((string-match "\\.class$" file1)
    (not (unless (equal (op:decompile-java-class file1 "dec-f1") (op:decompile-java-class file2 "dec-f2")) ;; if decompiled files are equal -> return t, otherwise return nil
           ;; (delete-file file1)
           ;; (delete-file file2)
           (with-current-buffer "dec-f1" (write-file (replace-regexp-in-string "\\.class$" ".java" file1)))
           (with-current-buffer "dec-f2" (write-file (replace-regexp-in-string "\\.class$" ".java" file2)))
           t))))) 


(defun op:delete-identical-and-extract-diff(dir1 dir2)
  (op:delete-identical-files dir1 dir2 #'op:equal-files?)) 

(defun op:cmp-2dirs (dir options)
  "compare first two dirs in a given directory recursively and delete identical files. uncompress files if possible
The following options are supported:
a - extract all files, the list *ignore-files-to-extract* is not used. usefull to compare signed *.war files, where even third-party jars are different in signature"
  (if (and options (string-match "a" options)) (setq *ignore-files-to-extract* '()))
  (let ((files (cddr (directory-files-and-attributes dir t))))
    (op:delete-identical-and-extract-diff (first (car files)) (first (cadr files)))))


(defun op:grep-error-and-group(dir &optional ignore-list)
  (let* ((nd (op:normalize-dir-name dir)) (grep-err-file (concat nd "grep-ERROR")) (inverted-grep "") (cmd ""))
    (unless (file-exists-p grep-err-file)
      (message (concat "grep ERROR in a dir " dir))
      (cd nd)
      (dolist (ignore ignore-list)
        (setq inverted-grep (concat inverted-grep " | grep -v '" ignore "'")))
      (setq cmd (concat "find " nd " -type f -name '*.log*' -print0 | xargs -0 -e grep -nH ' ERROR ' " inverted-grep " >" grep-err-file))
      (message (concat "launching shell command:" cmd))
      (shell-command cmd)
      (with-temp-buffer
        (insert-file-contents grep-err-file)
        (goto-char (point-min))
        (while (re-search-forward "\\(.*?\\)/\\([^/]+\\)$" nil t)
          (replace-match "./\\2"))
        (goto-char (point-min))
        (insert (concat "-*- mode: grep; default-directory: \"" dir "\" -*-\n\n"))
        (goto-char (point-max))
        (insert "\n\nGrep finished \n")
        (op:group-error-msg)
        (write-region (point-min) (point-max) grep-err-file)))))


(setq *op:split-size* 60)

(defun op:split-dir(dir &optional options ignore-list in-this-dir)
  "Recursively unzips all archive files and splits all files with the size in MB > *op:split-size*
the following options are supported:
e - grep ERROR and sends result to a file grep-ERROR
s - group sql statements
ignore-list is a list of strings to ignore in grep (if e is specified as an option)
A parameter in-this-dir is an internal parameter, it should be null."
  (message (concat "Processing dir: " dir (unless in-this-dir " in a new dir")))
  (let ((files (cddr (directory-files-and-attributes dir t))))
    (dolist (file files)
      (let* ((full-name (first file)) (is-dir (second file)) (name (file-name-nondirectory full-name)) (size (ninth file)) 
             (mkdir (if in-this-dir dir (concat full-name "_"))) (new-name (if in-this-dir full-name (concat mkdir "/" name))))
        (cond 
         (is-dir (op:split-dir full-name options ignore-list in-this-dir))
         ((op:is-archive-ext name)
          (op:split-dir (op:extract full-name) options ignore-list t)
          (delete-file full-name))
         ((string-match "health-check\\.log" name)
          (op:dired-plot-health-check nil full-name))
         ((string-match "server\\.log" name)
          (unless in-this-dir
            (make-directory mkdir)
            (rename-file full-name new-name))
          (when (> (/ size 1048576) *op:split-size*) 
            (op:split-log-file new-name nil)
            (delete-file new-name))
          (if (string-match "e" options) (op:grep-error-and-group mkdir ignore-list))
          (when (string-match "s" options) 
            (let ((log-files (cddr (directory-files-and-attributes mkdir t))) (maps))
              (dolist (log-file log-files)
                (with-current-buffer (get-buffer-create "server-log-tmp-buf")
                  (insert-file-contents-literally (first log-file))
                  (setq maps (op:count-sql-stmt-internal maps)))
                (kill-buffer "server-log-tmp-buf"))
              (op:print-sql-stmt-count (get-buffer-create "sql-stmt-count") maps)
              (with-current-buffer (get-buffer-create "sql-stmt-count")
                (write-file mkdir))
              (kill-buffer "sql-stmt-count")))))))))


;;(op:split-dir "T:/Oleg/cjc" "s" (list "Nibble") t)

(let ((dir "T:/Oleg/cmp/") (operation nil) (options nil) (ignore-list nil))
  (dolist (a command-line-args)
    (when (string-match "operation=\\(.*\\)" a) 
      (setq operation (intern-soft (match-string 1 a))))
    (when (string-match "dir=\\(.*\\)" a) 
      (setq dir (match-string 1 a)))
    (when (string-match "options=\\(.*\\)" a) 
      (setq options (match-string 1 a)))
    (when (string-match "ignore=\\(.*\\)" a)
      (setq ignore-list (cons (match-string 1 a) ignore-list))))
  (cond
   ((equal operation #'op:split-dir) (funcall operation dir options ignore-list))
   ((equal operation #'op:cmp-2dirs) (funcall operation dir options))
   (t (message "Usage: cmd>emacs.exe -batch -l cmp-dir.el -kill operation=[op:split-dir][op:cmp-2dirs] dir=dir-name options=es ignore=asdf1 ignore=asdf2"))))

;; (load-file (expand-file-name "~/emacs/scripts/cmp-dir.el"))

;;(op:split-dir "t:/Oleg/log" "es")
;;(op:extract-all-archives "t:\oleg\cmp")
;; d:\Soft\Emacs\emacs-23.2\bin\emacs.exe -batch -l cmp-dir.el -kill operation=op:cmp-2dirs dir="T:/Oleg/cmp/" options=a
;; d:\Soft\Emacs\emacs-23.2\bin\emacs.exe -batch -l cmp-dir.el -kill operation=op:split-dir dir="T:/Oleg/ins/24-04/" options=e
