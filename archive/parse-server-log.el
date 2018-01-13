(require 'cl)

(load-file (expand-file-name "~/emacs/idtv.el"))
(require 'idtv)
(load-file (expand-file-name "~/emacs/misc.el"))
(require 'misc)

;; 2009-05-08 02:02:29,089 INFO  [STDOUT] UnitOfWork(243797640)--Connection(1563974968)--INSERT  INTO AGT_PLY_EVENT (TIME_END_FRAME, EVENT_TRAFFIC_ID, DEVICE_CHANNEL_REF, EVENT_TYPE, TIME_END_GMT, FOREIGN_KEY, EVENT_LABEL, XID, TIME_START_FRAME, CONTAINER_SOURCE_REF, DEVICE_REF, EVENT_AUTOMATION_ID, TIME_START_GMT, DEVICE_DATA, EVENT_TO_DELETE, EVENT_SCHEDULED_STATE) VALUES (0.0, '1800613', '0704', 'PM', to_date('2009-05-08 06:03:36','YYYY-MM-DD HH24:MI:SS'), NULL, '0506 ??? B?? ??', 1713714, 0.0, '0506BBB', 'dev01', '', to_date('2009-05-08 06:00:00','YYYY-MM-DD HH24:MI:SS'), '{ programID="1800577"; assetName="0506BBB"; title="0506 ??? B?? ??"; FlagFirstEvent="Y"; triggerType="0"; deviceName="S6DEC2"; segmentXid="109028"; }', 'N', 'E')
;; 2009-05-08 02:02:29,090 INFO  [STDOUT] UnitOfWork(243797640)--Connection(1563974968)--INSERT  INTO AGT_PLY_EVENT (TIME_END_FRAME, EVENT_TRAFFIC_ID, DEVICE_CHANNEL_REF, EVENT_TYPE, TIME_END_GMT, FOREIGN_KEY, EVENT_LABEL, XID, TIME_START_FRAME, CONTAINER_SOURCE_REF, DEVICE_REF, EVENT_AUTOMATION_ID, TIME_START_GMT, DEVICE_DATA, EVENT_TO_DELETE, EVENT_SCHEDULED_STATE) VALUES (0.0, '1800612', '0704', 'PM', to_date('2009-05-08 06:03:48','YYYY-MM-DD HH24:MI:SS'), NULL, '?????:????? ?? 19', 1713713, 0.0, 'TRLAST19', 'dev01', '', to_date('2009-05-08 05:41:00','YYYY-MM-DD HH24:MI:SS'), '{ programID="1800576"; assetName="TRLAST19"; title="?????:????? ?? 19"; FlagFirstEvent="Y"; triggerType="0"; deviceName="S6DEC2"; segmentXid="107877"; }', 'N', 'E')

(defun op:get-field-value(field fields vals)
  (loop for idx from 0 for cf in fields until (equal (car cf) field) finally (return (car (nth idx vals)))))


(defun op:get-string-match(str regex match)
  (save-match-data
    (string-match regex str)
    (match-string match str)))


(defun op:lst<(l1 l2)
  (if (or (null l1) (null l2)) 
      nil
    (or (< (car l1) (car l2)) (and (equal (car l1) (car l2)) (op:lst< (cdr l1) (cdr l2))))))
        

(defun op:time<(t1 t2)
  (op:lst< (cdddr (reverse t1)) (cdddr (reverse t2))))
;; (op:time< (parse-time-string "2009-05-08 08:01:12") (parse-time-string "2009-05-08 06:03:36"))


(defun op:time<=(t1 t2)
  (or (op:time< t1 t2) (equal t1 t2)))


(defun op:grep-insert-agt-ply(logfile)
  (let ((grep-ply-event (concat logfile ".ple")))
    (unless (file-exists-p grep-ply-event)
      (setq default-directory (file-name-directory logfile))
      (shell-command (concat "find . -type f -name '" (file-name-nondirectory logfile) "' -print0 | xargs -0 -e grep -e 'INSERT[[:blank:]]\\+INTO[[:blank:]]\\+AGT_PLY_EVENT' >" grep-ply-event)))
    grep-ply-event))

(setq *op:ply-overlaps-compare-list-size* 50)

(defun op:find-agt-ply-event-overlaps(logfile)
  "finds overlaps in a logfile"
  (let ((kse-lst) (overlap-idx) (kse-top))
    (flet ((build-key-values(connection-id fields vals) ;; returns a list (event-key start-time end-time)
                            (list (concat 
                                   connection-id
                                   (op:get-field-value "DEVICE_CHANNEL_REF" fields vals)
                                   (op:get-field-value "DEVICE_REF" fields vals))
                                  (parse-time-string (op:get-string-match (op:get-field-value "TIME_START_GMT" fields vals) "('\\(.*?\\)'" 1))
                                  (parse-time-string (op:get-string-match (op:get-field-value "TIME_END_GMT"   fields vals) "('\\(.*?\\)'" 1))))
           (get-overlap-idx(kse)
                           (let ((result))
                             (loop for i from 0 for kse1-cons in kse-lst do
                                   (let ((kse1 (car kse1-cons)))
                                     (when (equal (car kse) (car kse1))
                                       (let ((s (cadr kse)) (e (caddr kse)) (s1 (cadr kse1)) (e1 (caddr kse1)))
                                         ;;                                        (when (and (equal s s1) (equal e e1)) (return (cons i "duplicate")))
                                         (when (or
                                                (and (op:time< s s1) (op:time< s1 e))
                                                (and (op:time< s e1) (op:time< e1 e))
                                                (and (op:time< s1 s) (op:time< e e1)))
                                           (push i result))))))
                             (nreverse result)))
           (update-kse-lst(fields vals)
                          (if vals (setq kse-lst 
                                         (append kse-lst (list (cons 
                                                                (build-key-values (op:get-string-match (op:curr-line) "\\(.*?\\)--Connection(\\([0-9]+\\))--" 2) fields vals)
                                                                (op:curr-line))))))))
      (with-temp-buffer
        (insert-file-contents (op:grep-insert-agt-ply logfile))
        (goto-char (point-min))
        (loop for i from 1 to *op:ply-overlaps-compare-list-size* for fwl = 0 then 1
              while (= 0 (forward-line fwl)) do
              (multiple-value-bind (fields vals) (op:parse-sql-insert (op:curr-line))
                (update-kse-lst fields vals)))
        (while kse-lst
          (setq kse-top (pop kse-lst) overlap-idx (get-overlap-idx (car kse-top)))
          (when overlap-idx
            (message (concat "----------------------------------------------------------------------------------------------------"))
            (message (cdr kse-top))
            (dolist (idx overlap-idx) 
              (message (cdr (nth idx kse-lst)))))
          (forward-line)
          (multiple-value-bind (fields vals) (op:parse-sql-insert (op:curr-line))
            (update-kse-lst fields vals)))))))


(defun op:find-agt-ply-event-non-null-frames(logfile)
  (with-temp-buffer
    (insert-file-contents (op:grep-insert-agt-ply logfile))
    (goto-char (point-min))
    (loop for i = 0 then (forward-line 1) while (equal i 0) do
          (multiple-value-bind (fields vals) (op:parse-sql-insert (op:curr-line))
            (when (not (and (equal "0.0" (op:get-field-value "TIME_START_FRAME" fields vals)) (equal "0.0" (op:get-field-value "TIME_END_FRAME" fields vals))))
              (message (op:curr-line)))))))
      

(let ((operation (intern-soft "op:find-agt-ply-event-overlaps")) (arg "u:/emacs/scripts/server.log"))
  (dolist (a command-line-args)
    (when (string-match "operation=\\(.*\\)" a) 
      (setq operation (intern-soft (match-string 1 a))))
    (when (string-match "arg=\\(.*\\)" a) 
      (setq arg (match-string 1 a))))
  (unless operation (message "Usage: cmd>emacs.exe -batch -l parse-server-log.el -kill dir=dir-name operation=[op:find-agt-ply-event-overlaps][op:find-agt-ply-event-non-null-frames] arg=t:/oleg/server.log"))
  (when (and operation arg) (funcall operation arg)))

;;(op:find-agt-ply-event-overlaps "u:/emacs/scripts/server.log")
;;(op:find-agt-ply-event-non-null-frames "u:/emacs/scripts/server.log")
;; d:\Soft\Emacs\emacs-22.3\bin\emacs.exe -batch -l parse-server-log.el -kill operation=op:find-agt-ply-event-overlaps arg=u:/emacs/scripts/server.log
;; d:\Soft\Emacs\emacs-22.3\bin\emacs.exe -batch -l parse-server-log.el -kill operation=op:find-agt-ply-event-non-null-frames arg=u:/emacs/scripts/server.log


