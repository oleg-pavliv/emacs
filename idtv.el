(require 'cl)
(require 'time-stamp)

(load-file (expand-file-name "~/emacs/misc.el"))
(require 'misc)


(defun op:count-msg (msg buffer &optional map num)
  "count messages msg in a buffer buf. returns a map msg->count"
  (let ((key) (buf (or buffer (current-buffer))) (result (or map (make-hash-table :test #'equal))))
    (save-excursion
      (with-current-buffer buf
        (goto-char (point-min))
        (while (re-search-forward msg nil t)
          (setq key (match-string-no-properties (or num 1)))
          (puthash key (+ 1 (gethash key result 0)) result))))
    result))


(defun op:count-sql-stmt-internal (&optional maps buffer)
  (list (op:count-msg "SELECT[[:blank:]]+.*?[[:blank:]]+FROM[[:blank:]]+\\(.*?\\)[[:blank:]]+WHERE[[:blank:]]+" buffer (and maps (car maps)))
        (op:count-msg "INSERT[[:blank:]]+INTO[[:blank:]]+\\(.*?\\)(" buffer (and maps (cadr maps))) 
        (op:count-msg "UPDATE[[:blank:]]+\\(.*?\\)[[:blank:]]+SET[[:blank:]]+" buffer (and maps (caddr maps)))
        (op:count-msg "DELETE[[:blank:]]+FROM[[:blank:]]+\\(.*?\\)[[:blank:]]+WHERE[[:blank:]]+" buffer (and maps (cadddr maps)))))


(defun op:print-sql-stmt-count (out-buf maps)
  (switch-to-buffer out-buf)
  (maphash (lambda (key value) (insert (format "%8i select %s\n" value key))) (car maps))
  (maphash (lambda (key value) (insert (format "%8i insert %s\n" value key))) (cadr maps))
  (maphash (lambda (key value) (insert (format "%8i update %s\n" value key))) (caddr maps))
  (maphash (lambda (key value) (insert (format "%8i delete %s\n" value key))) (cadddr maps))
  (sort-lines t (point-min) (point-max))
  (goto-char (point-min)))


(defun op:load-directory(dir buf &optional dir-opt permanent)
  (message (concat "loading " dir " ..."))
  (unless dir-opt (setq dir-opt "-lhIRA"))
  (when (not (get-buffer buf))
    (dired dir dir-opt) 
    (rename-buffer buf)
    (if permanent (op:mark-buffer-as-permanent buf)))
  (switch-to-buffer buf))


(defun op:load-xml-dirs (base-dir buf-name &optional customer)
  "load default xml dir and customer xml dir, optionally with quative"
  (unless customer (setq customer (read-string "Customer: ")))
  (let* ((flags "-lh") (flags-r (concat flags "R")))
    (op:load-directory base-dir buf-name flags)
    (op:mark-buffer-as-permanent buf-name)
    (dolist (base-customer '("" "default" "quative" "nmp"))
      (let* ((bcd (concat base-dir "/" base-customer)) (cd (concat bcd "/" customer))
             (bcdm (concat bcd "/DT3_GEN_MODULE")) (cdm (concat cd "/DT3_GEN_MODULE")) 
             (bcdt (concat bcd "/DT3_GEN_TYPES"))  (cdt (concat cd "/DT3_GEN_TYPES")))
        (if (and (file-exists-p bcdm) (or (equal "default" base-customer) (file-exists-p cdm))) ;; insert bcdm for default and for "quative", "nmp", ... but only if the customer belogns to it
            (dired-insert-subdir bcdm flags-r))
        (if (and (file-exists-p bcdt) (or (equal "default" base-customer) (file-exists-p cdt)))
            (dired-insert-subdir bcdt flags-r))
        (if (file-exists-p cdm)
            (dired-insert-subdir cdm flags-r))
        (if (file-exists-p cdt)
            (dired-insert-subdir cdt flags-r))))
    (goto-char (point-min))))


(defun op:build-shell (name path &optional java-version)
  (let ((buf name))
    (op:shell-in-dir (concat "c:/Perforce" path) buf)
    (op:mark-buffer-as-permanent buf)
    (switch-to-buffer buf)
    (op:send-cmd-to-shell (concat "JAVA_HOME=" java-version ";PATH=" java-version "/bin:$PATH\n") buf)
    (buffer-disable-undo buf)))


(when (equal "IDTV_WINDOWS" (getenv "IDTV"))
  
  (defun op:idtv3-build-shell ()
    (interactive)
    (op:build-shell "build-shell" "/LYSIS/CMS/BuildTools/REL/3.2STD/build/" "$JAVA5"))

  (defun op:cms4-build-shell ()
    (interactive)
    (op:build-shell "bms-build-shell" "/LYSIS/CMS/BuildTools/REL/4.0STD/build/" "$JAVA6-32")
    (op:build-shell "esb-build-shell" "/LYSIS/ESB/buildTools/REL/2.0STD/build/" "$JAVA6-32"))
  )



(defun op:plot-dat-file(file-dat file-plot &optional arg) 
  (let ((buf (if arg "hc-cache.plot" "hc-memthrd.plot")))
    (find-file-noselect (concat (getenv "home") "/emacs/" buf))
    (with-current-buffer buf
      (goto-char (point-min))
      (replace-string "$dat-file" (concat "\"" file-dat "\""))
      (write-file file-plot nil)))
  (kill-buffer (file-name-nondirectory file-plot))
  (if (string-match "[[:blank:]]" file-plot)
      (message (concat "cannot plot file '" file-plot "' which contains whitespaces"))
    (progn
      (shell-command (concat (getenv "utils") "/gnuplot/bin/wgnuplot -persist \"" file-plot "\" &") (time-stamp-string))
      (run-at-time "10 sec" nil (lambda(file) (delete-file file)) file-plot))))


(defun op:dired-plot-dat-file(&optional arg)
  (op:plot-dat-file (dired-get-file-for-visit) (concat (file-name-sans-extension (dired-get-file-for-visit)) ".plot") arg))


(defun op:dired-plot-health-check(&optional arg hc-file)
  (interactive)
  (let* ((file (if hc-file hc-file (dired-get-file-for-visit))) 
         (file-dat (concat file (if arg ".cdat" ".mtdat")))
         (file-plot (concat file ".plot"))
         (adaptors-from) (adaptors-to) (out-str)
         (buf-name-dat (file-name-nondirectory file-dat)))
    (shell-command (concat "grep \"Memory usage after garbage " (if arg "\\|com\\.lysis\\.idtv3\\.")  "\" '" file "'") buf-name-dat)
    (with-current-buffer buf-name-dat
      (goto-char (point-min))
      (replace-regexp ",.*?Memory usage after garbage collection :" " ")
      (goto-char (point-min))
      (replace-regexp "MB\\..*?Thread count :" " ")
      (goto-char (point-min))
      (replace-regexp " Up time.*?$" "")
      (goto-char (point-min))
      ;;       (replace-regexp "^....\\-..\\-.." "") ;; to replace yyyy-mm-dd
      (delete-other-windows)
      (write-file file-dat)
      (kill-buffer buf-name-dat))
    (unless hc-file (op:plot-dat-file file-dat file-plot arg))))



;;find 'd:/M7/20-05' -name 'server*' -exec mv '{}' '{}'.log  ';'
(defun op:dired-split-log-file(&optional arg)
  (interactive "P")
  (op:split-log-file (dired-get-file-for-visit) arg)
  (op:group-error-msg))


(defun op:split-log-file(file &optional arg)
  "Splits a file. If arg is not null it calls grep ' ERROR ' and displays result in a *grep* buffer."
  (message (concat "Splitting file " file "..."))
  (let* ((dir (file-name-directory file)) (name (file-name-nondirectory file)) (old-default-dir default-directory))
    (setq default-directory dir)
    (shell-command (concat "split --suffix-length=3 --lines 100000 '" file "' \"split-\""))
    (shell-command (concat "find '"  dir "' -name 'split-*' -exec mv '{}' '{}'-server.log  ';'"))
    (when arg
      (message "grep ERROR ")
      (lgrep "[[:blank:]]ERROR[[:blank:]]" "split-*" dir)
      (switch-to-buffer "*grep*")
      (delete-other-windows)
      (rename-buffer (concat "grep-" (file-name-sans-extension name))))
    (setq default-directory old-default-dir)))


(defun op:group-error-msg()
  "group several messages (stack dump) in one"
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward "Grep finished\\(.*\\)" nil t)
    (delete-region (match-beginning 0)  (match-end 0)))
  (goto-char (point-max))
  (insert "./split-zz.log:99999:1000-00-00 00:00:00,000 ERROR [text to be deleted]\n")
  (goto-char (point-min))
  (let ((delta 20) (prev-line-num -1) (prev-file-name "") (err-line "") (lysis-err-line "") (num-err 0) (num-lysis-err 0))
    (while (re-search-forward "\\(.*?\\):\\(.*?\\):\\(.*?\\)ERROR\\(.*?\\)$" nil t)
      ;; split-ab.log:14110:2008-06-19 11:52:09,896 ERROR [STDERR] java.net.ConnectException: Connection refused
      ;;(while (re-search-forward "\\./\\(.*?\\):\\(.*?\\):\\(.*?\\)ERROR\\(.*?\\)$" nil t)
      ;; ./xag:14152:2008-05-22 09:33:28,553 ERROR [com.lysis.idtv3.product.rules.DtvProductPackageRuleLevel2] 
      (let ((curr-line (op:get-buffer-match 0)) (file-name (op:get-buffer-match 1)) (line-num (string-to-number (op:get-buffer-match 2))) (err-info (op:get-buffer-match 4)))
        (delete-region (match-beginning 0) (+ 1 (match-end 0)))
        (if (and (>= (+ delta prev-line-num) line-num) (string= file-name prev-file-name))
            (progn
              (when (and (string-match "com\\.lysis\\.\\(.*?\\)" err-info) (< num-lysis-err 4))
                (setq lysis-err-line (if (= 0 num-lysis-err) curr-line (concat lysis-err-line err-info)) num-lysis-err (+ 1 num-lysis-err)))
              (when (< num-err 4)
                (setq err-line (if (= 0 num-err) curr-line (concat err-line err-info)) num-err (+ 1 num-err))))
          (progn
            (if (> num-lysis-err 0)
                (insert (concat lysis-err-line "\n"))
              (if (> num-err 0)
                  (insert (concat err-line "\n"))))
            (setq err-line curr-line num-err 1 lysis-err-line "" num-lysis-err 0)
            (when (string-match "com\\.lysis\\.\\(.*?\\)" err-info)
              (setq lysis-err-line curr-line num-lysis-err 1))))
        (setq prev-line-num line-num prev-file-name file-name)))))


(defun op:sort-stack-dump()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\n\t\t" nil t) (replace-match "NEWLINE-2-TABS"))
  (goto-char (point-min))
  (while (re-search-forward ".*\n" nil t)
    (let ((cnt (number-to-string (save-match-data (count-matches "NEWLINE-2-TABS" (match-beginning 0) (match-end 0))))))
      (replace-match (substring (concat "000" cnt " \\&") (- (length cnt) 1)))))
  (goto-char (point-min))
  (if (re-search-forward "0000 \\(Java HotSpot(TM).*\\)" nil t) (replace-match "9999 \\1")) ;; only one replacement, move this string to the top
  (sort-lines t (point-min) (point-max))
  (goto-char (point-min))
  (while (re-search-forward "^[[:digit:]]+\\(.*\\)" nil t) (replace-match "\\1"))
  (goto-char (point-min))
  (while (re-search-forward "NEWLINE-2-TABS" nil t) (replace-match "\n\t\t"))
  (goto-char (point-min)))



;; 2008-12-31 00:24:33,739 INFO  [STDOUT] UnitOfWork(429005202)--Connection(1787193990)--INSERT  INTO AGT_PLY_EVENT (TIME_END_FRAME, EVENT_TRAFFIC_ID, DEVICE_CHANNEL_REF, EVENT_TYPE, TIME_END_GMT, FOREIGN_KEY, EVENT_LABEL, XID, TIME_START_FRAME, CONTAINER_SOURCE_REF, DEVICE_REF, EVENT_AUTOMATION_ID, TIME_START_GMT, DEVICE_DATA, EVENT_TO_DELETE, EVENT_SCHEDULED_STATE) VALUES (0.0, '1383605', '0701', 'PM', to_date('2009-01-02 15:01:00','YYYY-MM-DD HH24:MI:SS'), NULL, '1231 ??? ??? ??', 1393554, 0.0, '1231ST1', 'dev01', '', to_date('2009-01-02 15:00:00','YYYY-MM-DD HH24:MI:SS'), '{ programID="1383583"; assetName="1231ST1"; title="1231 ??? ??? ??"; FlagFirstEvent="Y"; triggerType="0"; deviceName="S6DEC1"; segmentXid="101370"; }', 'N', 'E')
;; INSERT INTO DT3_EPG_FIELD( XID, NAME, DESCRIPTION, IS_LIST, IS_EDITABLE, SHOULD_GENERATE, IS_LOCALIZED, DATA_CLASS_NAME, IS_INHERITABLE ) VALUES ( 59, 'ProductCategory', 'Product category.', 0, 0, 0, 0, 'com.lysis.idtv3.epg.adaptors.DtvEpgStringData',1 );

(defun op:parse-sql-insert(str-insert)
  (save-excursion 
    (let* ((pos 0) (substr) (prev-pos) (vars) (values))
      (flet ((parse-vv(reg)
                      (let ((result))
                        (while (equal 0 (string-match (concat "[[:space:]]*" reg "[[:space:]]*[,)][[:space:]]*") substr))
                          ;; (setq stop (string-match (concat reg "[[:space:]]*)") substr))
                          (incf pos (match-end 0))
                          (push (cons (progn (string-match reg substr) (match-string 1 substr)) prev-pos) result)
                          (setq substr (substring str-insert pos))
                          (setq prev-pos pos))
                        result)))
        (when (string-match ".*?INSERT.*?(" str-insert)
          (setq pos (match-end 0) prev-pos pos)
          (setq substr (substring str-insert pos))
          (setq vars (parse-vv "\\([a-zA-Z0-9_]+\\)"))
          (when (string-match ".*?VALUES.*?(" substr)
            (setq prev-pos (incf pos (match-end 0)))
            (setq substr (substring str-insert pos))
            (setq values (parse-vv "\\(-?[0-9]+\\.?[0-9]*\\|'.*?'\\|to_date(.*?)\\|NULL\\|?\\)")))))
      (list vars values))))


(make-variable-buffer-local 'op:highlighted-arg)
(defun op:highlight-insert-args()
  (let ((new-mark (mark)))
    (save-excursion 
      (multiple-value-bind (vars vals) (op:parse-sql-insert (op:curr-line))
        (when vals
          (let ((pos (- (point) (line-beginning-position))) (vv-idx) (vv-lst))
            (flet ((calc-vv-idx(lst)
                               (loop for idx from 0 for vv in lst 
                                     for prev-vv = (- (line-end-position) (line-beginning-position)) then (cdr (nth (1- idx) lst))
                                     thereis (and (<= pos prev-vv) (<= (cdr vv) pos) idx))))
              (setq vv-idx (calc-vv-idx vals) vv-lst vars)
              (when (not vv-idx)
                (setq vv-idx (calc-vv-idx vars) vv-lst vals))
              (when vv-idx 
                (when op:highlighted-arg
                  (save-excursion
                    (goto-char (car op:highlighted-arg))
                    (cua-set-mark)
                    (goto-char (cdr op:highlighted-arg))
                    (markerpen-clear-region)))
                (setq new-mark (goto-char (+ (line-beginning-position) (cdr (nth vv-idx vv-lst)))))
                (cua-set-mark)
                (goto-char (+ (point) (length (car (nth vv-idx vv-lst)))))
                (setq op:highlighted-arg (cons (region-beginning) (region-end)))
                ;;                 (markerpen-clear-all-marks)
                (markerpen-mark-region 7)))))))
    (push-mark new-mark)))

(define-key (current-global-map) (kbd "C-M-<") (lambda () (interactive) (op:highlight-insert-args)))


(define-generic-mode 'jboss-log-mode
  nil 
  '("VALUES" "FROM" "WHERE" "AND")
  '("SELECT" "INSERT" "DELETE" "UPDATE") ;; are not keywords because start with '--' in the server.log file
  '("\\.log\\'")
  (list 
   (lambda () (set (make-local-variable 'buffer-read-only) 't))
   (lambda () (buffer-disable-undo))
   (lambda () (op:color-errors)))
  "Major mode for editing jboss log files.")

(define-generic-mode 'data-mode '("#") nil nil '("\\.data\\'") nil "Major mode for editing data files.")


(defun op:sql-beautify-region(beg end)
  "Beautify SQL in region between beg and END."
  (interactive "r")
  (let ((cmd "java SqlBeautify"))
    (save-excursion
      (shell-command-on-region beg end cmd nil t)
      (save-restriction
        (narrow-to-region (point) (mark))
        (goto-char (point-min))
        (while (re-search-forward "\n\\( *\\)," nil t)
          (replace-match ",\n\\1" nil nil))
        (widen)))))


(defun op:sql-beautify-buffer()
  "Beautify SQL in buffer."
  (interactive)
  (sql-beautify-region (point-min) (point-max))
  (goto-char (point-min))
  (while (re-search-forward "\n\\( *\\); *" nil t)
    (replace-match ";\n\\1" nil nil)))


(defun op:paste-log-sql(&optional arg)
  (interactive "P")
  (unless (string-match "sql" mode-name) 
    (switch-to-buffer (time-stamp-string)))
  (let ((pos (point)))
    (cua-paste nil)
    (narrow-to-region pos (point))
    (goto-char (point-min))
    (while (re-search-forward ".*?[0-9][0-9]:[0-9][0-9]:[0-9][0-9],[0-9][0-9][0-9]\\(.*?\\)\n" nil t)
      (if (string-match "TopLink Fine" (match-string 1))
          (save-excursion
            (unless (string-match "TopLink Fine" (op:curr-line))
              (move-end-of-line 0)
              (delete-char 1))
            (move-beginning-of-line nil))
        (replace-match "")))
    (goto-char (point-min))
    (while (re-search-forward ".*?--\\(INSERT\\|insert\\|UPDATE\\|update\\|DELETE\\|delete\\|SELECT\\|select\\) " nil t)
      (replace-match "NEW_LINE_PLACEHOLDER\\1 "))
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t) (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "NEW_LINE_PLACEHOLDER" nil t) (replace-match "\n\n"))
    (goto-char (point-min))
    (while (re-search-forward ".*?--Connection([0-9]+).*" nil t) (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "^.*?transaction$" nil t) (replace-match ""))
    (goto-char (point-min))
    (if arg (while (re-search-forward "\\(SELECT\\|UPDATE\\|INSERT\\|FROM\\|WHERE\\)" nil t) (replace-match "\n\\1")))
    (widen)))



(defun op:cat-remote (host file) 
  (let ((shell-buf (get-buffer-create (concat host file))))
    (shell shell-buf)
    (with-current-buffer shell-buf 
      (insert (concat "lftp -c 'open -u operator,Customer " host " -e \"cat " file "\"'")))
    (delete-other-windows)))

(defun op:cat-ds (host jboss) 
  (op:cat-remote host (concat "/soft/idtvsrv/" jboss "/server/default/deploy/lysis/idtv3-ds.xml")))

;; (defun op:cat-ds-402 (host) (interactive "sHost: ") (op:cat-ds host "jboss-4.0.2"))
(defun op:cat-ds-430 (host) (interactive "sHost: ") (op:cat-remote host "/soft/cmssoft/cms1/iDTV/current/deploy/idtv3-ds.xml"))

(defun op:cat-conf (host jboss) 
  (op:cat-remote host (concat "/soft/idtvsrv/" jboss "/bin/run.conf")))

;; (defun op:cat-conf-402 (host) (interactive "sHost: ") (op:cat-conf host "jboss-4.0.2"))
(defun op:cat-conf-430 (host) (interactive "sHost: ") (op:cat-remote host "/soft/cmssoft/cms1/jboss/current/bin/run.conf"))


(define-key global-map (kbd "C-c q") 
  (lambda () 
    (interactive) 
    (shell-command-to-string "find c:/work/FTP/CTR/licenses/ -type f -name '*.xml' -exec mv -T {} {}.success \\;") 
    (shell-command-to-string "find c:/work/FTP/CTR/vod-portal/ -type f -name '*.xml' -exec mv -T {} {}.success \\;")))


(defun op:iud-server-log (dir)
  "tail server.log only INSERT, UPDATE and DELETE statements excludeing tables from exclude-list"
  (interactive "DDeploy dir: ")
  (let ((shell-buf (get-buffer-create (concat dir "-log-iup")))
        (exclude-list (list "DT3_GEN_SEQUENCE" "UPDATE DT3_WOR_JOB_STEP" "UPDATE DT3_WOR_JOB" "DT3_SVC_LOCK" "UPDATE HDT3_VOD_VOD_ITEM SET WORKFLOW_STATUS")))
    (shell shell-buf)
    (with-current-buffer shell-buf
      (insert (concat "tail -f " 
                      dir 
                      "jboss-4.3.0.GA_CP06/server/default/log/server.log | grep --line-buffered -e \"--INSERT \\|--UPDATE \\|--DELETE \" " 
                      (mapconcat (lambda (x) (concat " | grep --line-buffered -v \"" x "\" ")) exclude-list ""))))
    (delete-other-windows)))



(provide 'idtv)


