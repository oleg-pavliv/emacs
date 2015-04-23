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


(defun op:cat-remote (host file) 
  (let ((shell-buf (get-buffer-create (concat host file))))
    (shell shell-buf)
    (with-current-buffer shell-buf 
      (insert (concat "lftp -c 'open -u operator,Customer " host " -e \"cat " file "\"'")))
    (delete-other-windows)))

(defun op:cat-ds (host jboss) 
  (op:cat-remote host (concat "/soft/idtvsrv/" jboss "/server/default/deploy/lysis/idtv3-ds.xml")))


(provide 'idtv)


