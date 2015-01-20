(require 'misc)

;; There are following bugs in sqlplus mode
;; - when a field contains a value with a leading minus sign (e.g. -1) then it cannot show it in the result buffer. 
;;   I think it inteprets the minus as a column separator
;;   Workaround: concatenate something before e.g. select '|' || data from hdt3_epg_data


;; CONSTRAINT_TYPE
;;----------------------------------------
;; C (check constraint on a table)
;; P (primary key)
;; U (unique key)
;; R (referential integrity)
;; V (with check option, on a view)
;; O (with read only, on a view)

;; to show CLOB columns: SET LONG 32000 
;; to show code of a stored procedure: select text from user_source where name = 'PROCEDURE-NAME' 
;; to set max rows: (setq sqlplus-pagesize 1000) 

(define-key (current-global-map) (kbd "C-M-2") 
  (lambda (arg) 
    (interactive "P") 
    (paredit-forward-slurp-sexp)
    (paredit-forward)))


(setq *op:sql-map-all-columns* '())
(setq *op:sql-map-useful-columns* '())
(setq *op:sql-primary-columns* '("xid" "name" "title" "public_id"))


(setq *op:useful-queries* '())

(push (list "epg field group" "select f.xid \"field.xid\", g.xid \"group.xid\", f.name \"field name\", g.name \"group\", data_class_name \"data_class_name\", is_editable \"edit?\", is_list \"list?\", is_localized \"loc?\", should_generate \"shld_gen?\" from dt3_epg_field f, dt3_epg_field_group g, dt3_epg_field_group_rel fg where f.xid = fg.epg_field_id and g.xid = fg.epg_field_group_id and lower(f.name) like '%rating%' order by f.xid;") *op:useful-queries*)
(push (list "catalogue vod item count" "select lpad(' ', 2*(level - 1)) || c.name || '     ' || (select count(*) from hdt3_vod_vod_item vi, hdt3_vod_item_node_link l where l.vod_item_id = vi.xid and l.catalogue_id = c.xid) from dt3_vod_catalogue c start with c.parent_id is null connect by prior c.xid = c.parent_id;") *op:useful-queries*)
(push (list "job step" "select st.xid \"step.xid\", j.xid \"job.xid\", vi.xid \"vi.xid\", vi.name, change_date, st.pos, st.status, step_version, stt.name, j.due_date \"due\", j.end_date \"end_date\" from dt3_wor_job_step st, dt3_wor_job_step_type stt, dt3_wor_job j, hdt3_vod_vod_item vi where st.type_id = stt.xid and st.job_id = j.xid and vi.xid = j.vod_item_id") *op:useful-queries*)
(push (list "vi job" "select vi.xid \"vi.xid\", vi.name, j.xid, j.change_step_date, j.catalogue_node_id \"node_id\", j.class, j.device_lock_id \"lock_id\", j.due_date, j.end_date, j.status, j.version, j.workflow_status \"wf_status\" from dt3_wor_job j, hdt3_vod_vod_item vi where j.vod_item_id = vi.xid") *op:useful-queries*)

(push (list "gen param" "select name, type \"type\", value \"value\" from dt3_gen_parameters  where lower(name) like '%%'") *op:useful-queries*)
(push (list "gen maps" "select name, key \"key\", value \"value\" from dt3_gen_maps  where lower(name) like '%%'") *op:useful-queries*)
(push (list "algo param set" "select m.xid \"meta.xid\", ps.xid \"ps.xid\", name \"ps.name\", algorithm_name, key, value, type, description, parent_algo_param_set_id from dt3_sys_algo_par_set_metadata m inner join dt3_sys_algo_par_set ps on m.algo_param_set_id = ps.xid where lower(algorithm_name) like '%opentv%';") *op:useful-queries*)

(push (list "dev meta" "select d.xid \"dev.xid\", d.name \"dev.name\", m.xid, m.key, m.value  from dt3_sys_dev_metadata m, dt3_sys_device d where m.device_id = d.xid and lower(d.name) like '%pack%'") *op:useful-queries*)
(push (list "vi meta" "select vi.xid \"vi.xid\", vi.name, vi.licensing_end_gmt, m.xid \"meta.xid\", m.key, m.value from hdt3_vod_vod_item_metadata m, hdt3_vod_vod_item vi where m.vod_item_id = vi.xid") *op:useful-queries*)

(push (list "epg products" "select p.xid, p.name, p.public_id, p.business_product_type \"bpt\", p.cas_product_id \"cas_prid\", p.nominal_price \"price\", p.preview_time \"preview\", p.validty_start_date_gmt \"validty_start_date\", p.validity_end_date_gmt \"validity_end_date\", p.version \"version\" from hdt3_pro_product p inner join hdt3_pro_product_link pl on p.xid = pl.product_id inner join hdt3_sch_epg_slot s on s.xid = pl.epg_slot_id inner join dt3_cha_schedule c on s.schedule_channel_id = c.xid where s.start_date_gmt > to_date('2011-06-24','YYYY-MM-DD') and s.end_date_gmt < to_date('2011-12-31','YYYY-MM-DD') and c.short_name like '%%'") *op:useful-queries*)

(push (list "update casid epg prod" "update hdt3_pro_product set cas_product_id = substr(public_id, 4, 7) where xid in (select p.xid from hdt3_pro_product p inner join hdt3_pro_product_link pl on p.xid = pl.product_id inner join hdt3_sch_epg_slot s on s.xid = pl.epg_slot_id inner join dt3_cha_schedule c on s.schedule_channel_id = c.xid where s.start_date_gmt > to_date('2011-06-24','YYYY-MM-DD') and s.end_date_gmt < to_date('2011-12-31','YYYY-MM-DD') and c.short_name like '%%')" ) *op:useful-queries*)

(push (list "update last vod item meta" "update hdt3_vod_vod_item_metadata set value = '1' where xid = 
  (select xid from hdt3_vod_vod_item_metadata where key = 'OpenTVDeletedDate' and vod_item_id = 
        (select vod_item_id from hdt3_vod_vod_item_metadata where xid = (select max(xid) from hdt3_vod_vod_item_metadata)));\ncommit;") *op:useful-queries*)



(define-key orcl-mode-map "\C-c\C-q" 
  (lambda (arg)
    "Use ido to select a useful query"
    (interactive "P")
    (let ((query-name (ido-completing-read "Choose query: " (mapcar 'first *op:useful-queries*) nil t)))
      (insert (cadr (assoc query-name *op:useful-queries*))))))





(defun sqlplus-read-connect-string (&optional connect-string default-connect-string)
  "Customized to remove downcase. From oracle 11 pwd is case-censitive"
  (unless default-connect-string
    (let ((inactive-connect-strings (cdr (sqlplus-divide-connect-strings))))
      (setq default-connect-string
            (some (lambda (pair)
                    (when (member (car pair) inactive-connect-strings) (car pair)))
                  sqlplus-connect-strings-alist))))
  (let* ((cs (or connect-string 
                 (read-string (format "Connect string%s: " (if default-connect-string (format " [default %s]" default-connect-string) ""))
                              nil 'sqlplus-connect-string-history default-connect-string)))
         (pair (refine-connect-string cs))
         (refined-cs (car pair))
         (password (cdr pair))
         (was-password password)
         (association (assoc refined-cs sqlplus-connect-strings-alist)))
    (unless (or password current-prefix-arg)
      (setq password (cdr association)))
    (unless password
      (setq password (read-passwd (format "Password for %s: " cs))))
    (unless was-password
      (if (string-match "@" cs)
          (setq cs (replace-match (concat "/" password "@") t t cs))
        (setq cs (concat cs "/" password))))
    (list cs refined-cs)))


(defadvice sqlplus-mode (after sqlplus-mode-rename activate)
  (setq mode-name "sqlplus")  )

(defadvice sqlplus-send-current (before write-buf-sqlplus-send-current activate)
  (write-file (op:sql-get-cmd-file-name (buffer-name))))


;; (defadvice sqlplus-send-current (after shrink-sqlplus-send-current activate)
;;   (shrink-window-if-larger-than-buffer))


(defadvice sqlplus-verify-buffer (before sqlplus-verify-buffer-and-restore-connection activate)
  (unless (get-buffer-process (sqlplus-get-process-buffer-name connect-string))
    (sqlplus connect-string)))

(defun op:init-session (connect-string)
  ;; (sqlplus-execute connect-string "alter session set NLS_DATE_FORMAT='YYYY-MM-DD';" nil nil)
  ;; (sqlplus-execute connect-string "alter session set NLS_TIMESTAMP_FORMAT='YYYY-MM-DD HH24:MI:SS';" nil nil)
  (sqlplus-execute connect-string "alter session set NLS_DATE_FORMAT='YYYY-MM-DD';" nil nil)
  (sqlplus-execute connect-string "alter session set NLS_TIMESTAMP_FORMAT='YYYY-MM-DD HH24:MI:SS';" nil nil)
  (sqlplus-execute connect-string "set LONG 32000;" nil nil)
)

(defadvice sqlplus (around sqlplus-split-vertically activate)
  (let ((orign-split-width-threshold split-width-threshold) (cs connect-string) (buf))
    (setq split-width-threshold nil)
    ad-do-it
    (op:init-session cs)
    (setq buf (sqlplus-get-input-buffer-name cs))
    (with-current-buffer buf
      (let ((cmd-file (op:sql-get-cmd-file-name buf)))
        (if (file-exists-p cmd-file) (insert-file-contents cmd-file))))
    (setq split-width-threshold orign-split-width-threshold)))


(defmacro op:sql-execute-col-width (arg &rest form)
  "arg is a max col width"
  `(let ((old-val sqlplus-select-result-max-col-width))
     (setq sqlplus-select-result-max-col-width ,arg)
     ,@form
     (run-at-time "5 sec" nil (lambda () (setq sqlplus-select-result-max-col-width old-val)))))

;;(macroexpand '(op:sql-execute-col-width arg (sqlplus-send-current nil)))


(defun op:sql-current-schema (filename &optional unused)
  (interactive (list 
                (minibuffer-with-setup-hook
                    (lambda ())
                  (read-file-name "Open schema: " (concat (getenv "home") "/emacs/scripts/schemas/") nil t)) t))
  (find-file filename)
  (setq *op:sql-map-all-columns* '())
  (goto-char (point-min))
  (while (re-search-forward "^[[:blank:]]*\n" nil t) (replace-match "" nil nil))
  (goto-char (point-min))
  (let ((table) (columns) (cont))
    (while (re-search-forward "^--\\([a-zA-Z0-9_-]+\\)[[:blank:]]*" nil t)
      (setq table (match-string-no-properties 1))
      (setq columns nil)
      (setq cont t)
      (while cont
        (forward-line)
        (let ((line (op:curr-line)))
          (if (string-match "^\\([a-zA-Z0-9_-]+\\)[[:blank:]]+\\([a-zA-Z0-9()_-]+\\)[[:blank:]]+\\([0-9]+\\)" line)
              (push (list (match-string 1 line) (match-string 2 line) (match-string 3 line)) columns)
            (setq cont nil))))
      (push (list table (reverse columns)) *op:sql-map-all-columns*))))


;; execute statement prefixed by a col width
(define-key orcl-mode-map "\C-c\C-d" 
  (lambda (arg) 
    "execute statement prefixed with column width"
    (interactive "P") 
    (op:sql-execute-col-width arg (sqlplus-send-current nil))))


(defun op:sql-copy-result (to-buf from-char str-beg &optional str-end)
  (let ((indices "") (beg) (s-end (or str-end "selected\\. Elapsed")))
    (with-current-buffer (sqlplus-get-output-buffer-name sqlplus-connect-string)
      (save-excursion
        (save-restriction
          (narrow-to-region from-char (point-max))
          (goto-char (point-min))
          (when (re-search-forward str-beg nil t)
            (move-beginning-of-line 1)
            (setq beg (point))
            (when (re-search-forward s-end nil t)
              (move-beginning-of-line 1)
              (setq indices (buffer-substring beg (point)))))
          (widen))))
    (with-current-buffer to-buf
      (insert (concat "\n\n" indices)))))



(defun op:sql-get-table-definition (&optional table-name)
  "show table definitions as create table(...) with constraints and indexes in a separate buffer"
  (let* ((name (or table-name (upcase (thing-at-point 'symbol))))
         (sql-con (format "SELECT CONSTRAINT_NAME, CONSTRAINT_TYPE \"T\", SEARCH_CONDITION, R_CONSTRAINT_NAME, STATUS, DEFERRABLE, DEFERRED, INDEX_NAME FROM ALL_CONSTRAINTS WHERE TABLE_NAME = '%s';" name))
         (sql-idx (format "SELECT DBMS_METADATA.GET_DEPENDENT_DDL('INDEX','%s') FROM DUAL;" name))
         (from (with-current-buffer (sqlplus-get-output-buffer-name sqlplus-connect-string) (point-max)))
         (prolog-commands (list 
                           "SET PAGESIZE 0"
                           "SET PAGES 999"
                           "COLUMN DBMS_METADATA.GET_DEPENDENT_DD FORMAT A3000"
                           "SET LONG 10000"
                           "SET LINESIZE 10000")))
    (sqlplus-get-source sqlplus-connect-string name "TABLE")
    (op:sql-execute-col-width 10000 (sqlplus-execute sqlplus-connect-string sql-idx nil prolog-commands))
    (op:sql-execute-col-width 10000 (sqlplus-execute sqlplus-connect-string sql-con nil nil))
    (run-at-time "1 sec" nil (lambda (src-buf to-buf from-char)
                               (with-current-buffer (get-buffer-create to-buf)
                                 (insert-buffer-substring src-buf)
                                 (kill-buffer src-buf))
                               (op:sql-copy-result to-buf from-char "^ DBMS_METADATA.GET_DEPENDENT_DD")
                               (op:sql-copy-result to-buf from-char "^ CONSTRAINT_NAME")
                               (pop-to-buffer to-buf)
                               (delete-other-windows))
                 (concat name ".sql") (concat name (time-stamp-string)) from)))



(define-key orcl-mode-map "\C-c\C-s" 
  (lambda (full) 
    "get table definition"
    (interactive "P")
    (if full
        (op:sql-get-table-definition)
      (message (mapconcat 'identity (op:sql-get-table-all-columns (upcase (thing-at-point 'symbol))) " ")))))


;; grobal variable because op:sql-try-expand-column can be called multiple times
;; first time this variable is initialized then it is just re-used
(setq current-tables-or-columns-list nil)

(defun op:fuzzy-match (search-string list)
  (let ((ss-reg-fuzzy (mapconcat 'string search-string ".*")))
    (delete-if 'null 
               (delete-dups (append
                             (sort (mapcar (lambda (s) (if (string-match search-string s) s)) list) 'string-lessp) ;; include exact matches first
                             (sort (mapcar (lambda (s) (if (string-match ss-reg-fuzzy s) s)) list) 'string-lessp)))))) ;; include fuzzy matches after


(defun op:sql-try-expand-column (old)
  (when (equal mode-name "sqlplus")
    (unless old
      (he-init-string (save-excursion (backward-word 1) (point)) (point))
      (if (string-match "dt3" he-search-string)
          (setq current-tables-or-columns-list (op:sql-get-all-tables))
        ;; search all dt3_ strings (case-insensitive) in the current line and put all columns (low and upper case) in current-tables-or-columns-list
        (save-excursion
          (move-beginning-of-line nil)
          (setq current-tables-or-columns-list nil)
          (while (re-search-forward "\\(\\b[hH]?[dD][tT]3_\\w+\\)" (save-excursion (move-end-of-line nil) (point)) t)
            (let* ((table (upcase (match-string 1))) 
                   (columns (op:sql-get-table-all-columns table)))
              (setq current-tables-or-columns-list (append current-tables-or-columns-list columns (mapcar #'downcase columns)))))))
      ;;(setq he-expand-list (sort (all-completions he-search-string (mapcar 'list current-tables-or-columns-list)) 'string-lessp)))
      (setq he-expand-list (op:fuzzy-match he-search-string current-tables-or-columns-list)))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (when old (he-reset-string))
      (he-substitute-string (car he-expand-list))
      (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
      (setq he-expand-list (cdr he-expand-list))
      t)))



(defun op:sql-get-cmd-file-name (buf)  (concat "c:/temp/sqlplus/"  (if (string-match "\\([a-zA-Z0-9_-]*\\).*" buf) (match-string 1 buf) "sqlplus-buf")))


(defun op:sql-get-all-tables ()
  (loop for tbl in *op:sql-map-all-columns* collect (car tbl)))


(defun op:sql-get-table-all-columns (table)
  (if *op:sql-map-all-columns*
      (loop for col in (cadr (assoc (downcase table) *op:sql-map-all-columns*)) collect (car col))))


(defun op:sql-concat-comma (str1 str2) (concat str1 (if str1 ", " "") str2))


(defun op:sql-convert-column (column)
  (let ((db-name (first column)) (tbl-name (second column)) (convert (third column)))
    (cond
     ((and (equal "date-time" convert))
      (concat "to_char(" db-name ", 'YYYY-MM-DD HH24:MI:SS') \"" tbl-name "\""))
     (t (concat db-name " \"" tbl-name "\"")))))


(defun op:sql-replace-star-with-useful-columns ()
  (if *op:sql-map-all-columns*
      (let* ((table) (numrows) (all-columns) (rgn (sqlplus-mark-current)) (sql-stmt (buffer-substring-no-properties (car rgn) (cdr rgn))) (stmt-rest) (result))
        (when (string-match "^[[:blank:]]*\\([0-9]*\\)select[[:blank:]]+\\*[[:blank:]]+from[[:blank:]]+\\([a-zA-Z0-9_-]+\\)\\(.*\\)" sql-stmt)
          (setq numrows (match-string 1 sql-stmt) table (downcase (match-string 2 sql-stmt)) all-columns (op:sql-get-table-all-columns table) stmt-rest (match-string 3 sql-stmt))
          (dolist (col *op:sql-primary-columns*)
            (when (member col all-columns) (setq result (op:sql-concat-comma result col))))
          (dolist (col (cdr (assoc table *op:sql-map-useful-columns*)))
            (when (and (cdr col) (not (member (car col) *op:sql-primary-columns*)) (member (car col) all-columns))
              (setq result (op:sql-concat-comma result (op:sql-convert-column col)))))
          (unless (assoc table *op:sql-map-useful-columns*)
            (dolist (col all-columns)
              (when (not (member col *op:sql-primary-columns*))
                (setq result (op:sql-concat-comma result col)))))
          (setq result (if result (concat "\n\n" numrows "select " result " from " table " " stmt-rest "\n\n" ) sql-stmt)))
        (when result 
          (save-excursion
            (kill-region (car rgn) (cdr rgn))
            (insert result))
          (forward-line 2)
          (move-end-of-line 1)))
    (message "call op:sql-current-schema")))


(defun op:sql-insert-col-names ()
  (let ((result) (pos (point)))
    (save-excursion
      (beginning-of-line)
      (narrow-to-region (point) pos)
      (when (re-search-forward "[[:blank:]]*insert[[:blank:]]+into[[:blank:]]+\\([a-zA-Z0-9_-]+\\)" nil t)
        (dolist (col (op:sql-get-table-useful-columns (op:get-buffer-match 1)))
          (setq result (op:sql-concat-comma result col))))
      (widen))
    (when result 
      (insert (concat "(" result ") values ();"))
      (backward-char)(backward-char))))


(defun op:sql-replace-functions ()
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (narrow-to-region (point) pos)
      (when (re-search-forward "\\(to_date()\\)" nil t)
        (replace-match "to_date('2010-12-31 23:59:59','YYYY-MM-DD HH24:MI:SS')"))
      (widen))
    (backward-sexp)))


(defun op:sql-replace-star-process-columns (select process-cols)
  "This function is obsolete. Using *op:sql-map-all-columns* map is a preferable way to replace columns.
The function replaces * with all columns. Then it calls (process-cols columns buf from-where)
columns is a column list buf is the current buffer from-where is a from where clause"
  (when (string-match "select[[:blank:]]+\\*[[:blank:]]+from[[:blank:]]+\\(\\w+\\)\\(.*\\)" select)
    (let* ((buf (current-buffer))
           (name (match-string 1 select))
           (from-where (concat " from " name " " (match-string 2 select)))
           (sql (format (concat "select column_name || ',' from all_tab_columns where owner = user and table_name = '%s' order by column_name;") (upcase name)))
           (prolog-commands (list "set echo off"
                                  "set newpage 0"
                                  "set space 0"
                                  "set pagesize 0"
                                  "set feedback off"
                                  "set long 4000"
                                  "set longchunksize 4000"
                                  "set wrap on"
                                  "set heading off"
                                  "set trimspool on"
                                  "set linesize 4000"
                                  "set timing off"))
           (context-options (list (cons :dont-parse-result 'dont-parse)
                                  (cons :source-text nil)
                                  (cons :source-type "TABLE")
                                  (cons :source-name name)
                                  (cons :source-extension "sql")
                                  (cons :result-function 'sqlplus-get-source-function))))
      (sqlplus-execute sqlplus-connect-string sql context-options prolog-commands t t)
      (sqlplus-execute sqlplus-connect-string (format "select '%s' from dual;" sqlplus-end-of-source-sentinel) context-options prolog-commands t t)
      (run-at-time "1 sec" nil (lambda (process-cols src-buf to-buf from-where)
                                 (goto-char (point-min))
                                 (let* ((columns-str (buffer-substring-no-properties (re-search-forward "(\n") (- (re-search-forward "\n)") 2)))
                                        (columns (split-string columns-str "[,\n]+" t)))
                                   (kill-buffer src-buf)
                                   (funcall process-cols columns to-buf from-where)))
                   process-cols (concat (upcase name) ".sql") buf from-where))))

;; the old way of doing things
;; (defun op:sql-replace-vselect (select)
;;   (when (string-match "vselect" select)
;;     (op:sql-replace-star-process-columns select 
;;                                          (lambda (columns buf from-where)
;;                                            (let* ((result (concat "select ")) (r2000 "") (first t))
;;                                              (dolist (c columns) ;; split result on several lines because of restriction 2499 char per line
;;                                                (setq r2000 (concat r2000 (unless first " || ") (concat "'" c " ' || " c " || CHR(10) ")))
;;                                                (setq first nil)
;;                                                (when (> (length r2000) 2000)
;;                                                  (setq result (concat result r2000 "\n"))
;;                                                  (setq r2000 "")))
;;                                              (with-current-buffer buf 
;;                                                (move-beginning-of-line nil)
;;                                                (kill-line)
;;                                                (insert (concat result r2000 from-where))))))))


(defun op:sql-replace-vselect (select)
  (when (string-match "vselect[[:blank:]]+\\*[[:blank:]]+from[[:blank:]]+\\(\\w+\\)\\(.*\\)" select)
    (if *op:sql-map-all-columns*
        (let* ((table (match-string 1 select)) (where (match-string 2 select)) 
               (result "select ") (r2000 "") (first t) 
               (columns (op:sql-get-table-all-columns table)))
          (dolist (c columns) ;; split result on several lines because of restriction 2499 char per line
            (setq r2000 (concat r2000 (unless first " || ") (concat "'" c " ' || " c " || CHR(10) ")))
            (setq first nil)
            (when (> (length r2000) 2000)
              (setq result (concat result r2000 "\n"))
              (setq r2000 "")))
          (move-beginning-of-line nil)
          (kill-line)
          (insert (concat result r2000 " from " table " " where)))
      (message "call op:sql-current-schema"))))


(defun op:sql-first-rows (select)
  (when (string-match "\\([0-9]+\\)select[[:blank:]]+\\(.*?\\)\\bfrom[[:blank:]]+\\(\\w+\\)\\(.*\\)" select)
    (if *op:sql-map-all-columns*
        (let* ((count (match-string 1 select)) (cols (match-string 2 select)) (table (match-string 3 select)) (where (match-string 4 select)) 
               (columns (mapconcat (lambda (x) x) (op:sql-get-table-all-columns table) ",")))
          (move-beginning-of-line nil)
          (kill-line)
          (insert (concat "select " cols " from \n  (select rownum rnum," columns " from (select * from " table " " where ") \nwhere rownum <= " count ") where rnum > 0")))
      (message "call op:sql-current-schema"))))


(defun op:sql-next-rows (where)
  (when (string-match "where rownum <= \\([0-9]+\\)) where rnum > \\([0-9]+\\)" where)
    (let ((num1 (string-to-number (match-string 1 where))) 
          (num2 (string-to-number (match-string 2 where))))
      (move-beginning-of-line nil)
      (kill-line)
      (insert (concat "where rownum <= " (number-to-string (- (+ num1 num1) num2)) ") where rnum > " (number-to-string num1))))))


;; ;; SELECT xid, title, public_id, approx_duration "duration", class "class", owner_id "owner_id", type_subtype_list "type_subtype_list", version "version"
;; ;; FROM
;; ;;   (SELECT rownum rnum, xid, title, public_id, approx_duration , class, owner_id, type_subtype_list, version
;; ;;    FROM  (SELECT xid, title, public_id, approx_duration , class , owner_id , type_subtype_list , version  FROM dt3_con_content )
;; ;;   WHERE rownum <= 200
;; ;;   )
;; ;; WHERE rnum > 100;

;;                                                     )
;;                                                   (with-current-buffer to-buf
;;                                                     (insert "select ")
;;                                                     (dolist (c columns) (insert (concat c ",")))
;;                                                     (insert from-where))))))


(defun op:sql-replace-users (stmt)
  (when (string-match "USERS[[:blank:]]+select[[:blank:]]+\\(.*?\\)from[[:blank:]]+\\(\\w+\\)\\(.*\\)" stmt)
    (let* ((fields (match-string 1 stmt)) 
           (table (match-string 2 stmt)) 
           (where (replace-regexp-in-string "'" "''" (match-string 3 stmt))))
      (move-beginning-of-line nil)
      (kill-line)
      (insert (concat "set linesize 4000\nselect 'select '''||username||''', " fields " from '||username|| '." table " " where ";' from all_users  where  username like '%OFFAIR%' order by created desc ")))))


(defun op:sql-contextual-expand ()
  (op:sql-replace-vselect (op:curr-line))
  (op:sql-replace-star-with-useful-columns)
  (op:sql-replace-users (op:curr-line))
  (op:sql-next-rows (op:curr-line))
  (op:sql-first-rows (op:curr-line))
  (op:sql-insert-col-names)
  (op:sql-replace-functions)
  )



(define-key orcl-mode-map "\C-c\C-c" 
  (lambda () "contextual expand" (interactive) (op:sql-contextual-expand)))

(define-key orcl-mode-map "\C-c\C-b" 
  (lambda () 
    "extract blob data"
    (interactive)
    (let ((col (thing-at-point 'symbol)))
      (kill-region (beginning-of-thing 'symbol) (end-of-thing 'symbol))
      (insert (concat "utl_raw.cast_to_varchar2(dbms_lob.substr(" col "))")))))


(defun op:sql-create-table-to-map ()
  (interactive)
  (when (re-search-forward "create[[:blank:]]+table[[:blank:]]+\\([a-zA-Z0-9_-]+\\)" nil t)
    (let ((tbl-name (op:get-buffer-match 1)) (col-name) (pos))
      (beginning-of-line) (kill-line) (kill-line) (kill-line) (kill-line)
      (insert (concat "(push '(\"" tbl-name "\" . (\n"))
      (narrow-to-region (point) (re-search-forward "^);"))
      (goto-char (point-min))
      (while (re-search-forward "[[:blank:]]+\\([a-zA-Z0-9_-]+\\).*?" nil t)
        (setq col-name (op:get-buffer-match 1))
        (beginning-of-line) (kill-line) (kill-line)
        (insert (concat "(\"" col-name "\" \"" col-name "\")\n")))
      (widen)
      (beginning-of-line) (kill-line) (kill-line)
      (insert ")) *op:sql-map-useful-columns*)")
      (setq pos (point))
      (backward-sexp) (indent-region (point) pos))))



(op:sql-current-schema (concat (getenv "home") "/emacs/scripts/schemas/dev-mgmt"))
(kill-buffer "dev-mgmt")
;;------------------------------------------------------------------------------------------------------------------------------------------------------------------

(push '("dt3_cha_schedule" . (
                              ("description" "description")
                              ("language_id" "language_id")
                              ("long_name" "long_name")
                              ("service_id" "service_id")
                              ("short_name" "short_name")
                              ("start_of_day_hours" "start_of_day_hours")
                              ("subtype" "subtype")
                              ("tv_channel" "tv_channel")
                              ("type" "type")
                              ("version" "version")
                              )) *op:sql-map-useful-columns*)



;; (push '("hdt3_epg_data" . (
;;                            ("data" "data")
;;                            ("field_id" "field_id")
;;                            ("locale_id" "locale_id")
;;                            ("owner_id" "owner_id")
;;                            ("pos" "pos")
;;                            ("version" "version")
;;                            )) *op:sql-map-useful-columns*)


(push '("dt3_epg_field" . (
                           ("data_class_name" "data_class_name") 
                           ("is_editable" "edit?") 
                           ("is_list" "list?") 
                           ("is_localized" "loc?") 
                           ("should_generate" "shld_gen?") 
                           )) *op:sql-map-useful-columns*)


(push '("dt3_ast_asset" . (
                           ("asset_data_id" "data_id") 
                           ("asset_size" "size") 
                           ("cas_content_id" "cas_cnt_id") 
                           ("content_id" "cnt_id") 
                           ("duration" "duration") 
                           ("format" "format") 
                           ("location" "location") 
                           ("storage_device_id" "device_id") 
                           ("transmission_tag" "trsm_tag") 
                           ("type" "type") 
                           ("version" "version") 
                           )) *op:sql-map-useful-columns*)




(push '("dt3_exp_exported_object" . (
                                     ("end_validity_date" "end_validity_date") 
                                     ("export_context" "export_context") 
                                     ("export_name" "export_name") 
                                     ("last_access_date" "last_access_date") 
                                     ("referenced_object_class" "referenced_object_class") 
                                     ("referenced_object_id" "ref_id") 
                                     ("start_validity_date" "start_validity_date") 
                                     )) *op:sql-map-useful-columns*)


(push '("dt3_con_content" . (
                             ("approx_duration" "duration") 
                             ("class" "class") 
                             ("owner_id" "owner_id") 
                             ("type_subtype_list" "type_subtype_list") 
                             ("version" "version") 
                             )) *op:sql-map-useful-columns*)


(push '("dt3_vod_catalogue" . (
                               ("catalogue_id" "ctlg_id") 
                               ("catalogue_node_type" "node_type") 
                               ("company_id" "company_id") 
                               ("is_obsolete" "obslt?") 
                               ("is_technical" "tech?") 
                               ("next_id" "next_id") 
                               ("nodetype" "nodetype") 
                               ("parent_id" "parent_id") 
                               ("previous_id" "previous_id") 
                               ("product_id" "product_id") 
                               ("type" "type") 
                               ("version" "version") 
                               ("modification_date" "modifi_date") 
                               ("hidden" "hidden") 
                               )) *op:sql-map-useful-columns* )


(push '("hdt3_sch_programme_slot" . (
                                     ("end_date_gmt" "end_date_gmt" "date-time") 
                                     ("fixed_start_date_gmt" "fixed_start_date_gmt" "date-time")
                                     ("pattern_id" "pattern_id") 
                                     ("schedule_channel_id" "channel_id")
                                     ("start_date_gmt" "start_date_gmt" "date-time") 
                                     )) *op:sql-map-useful-columns*)

(push '("dt3_gen_maps" . (
                          ("key" "key")
                          ("value" "value")
                          )) *op:sql-map-useful-columns*)


(push '("dt3_gen_parameters" . (
                                ("type" "type") 
                                ("value" "value") 
                                )) *op:sql-map-useful-columns*)


(push '("hdt3_vod_vod_item" . (
                               ("creation_date" "creation_date") 
                               ("licensing_end_gmt" "licensing_end_gmt") 
                               ("licensing_start_gmt" "licensing_start_gmt") 
                               ("marketable_class" "mrktbl_cls") 
                               ("preview_date_gmt" "preview_date_gmt") 
                               ("version" "version") 
                               ("video_content_id" "video_content_id") 
                               ("hidden" "hdn?") 
                               ("type" "type") 
                               ("workflow_status" "wf_st")
                               )) *op:sql-map-useful-columns*)



(push '("hdt3_sch_epg_slot" .  (
                                ("dvb_event_id" "dvb_id") 
                                ("epg_created" "epg_created") 
                                ("epg_field_group_id" "epg_fld_grp_id") 
                                ("main_event_start_date_gmt" "main_start_date") 
                                ("schedule_channel_id" "channel_id") 
                                ("start_date_gmt" "start_date_gmt" "date-time") 
                                ("end_date_gmt" "end_date_gmt" "date-time") 
                                )) *op:sql-map-useful-columns*)


(push '("hdt3_pro_product" . (
                              ("business_product_type" "bpt") 
                              ("cas_product_id" "cas_prid") 
                              ("is_impulsive" "impl?") 
                              ("is_ordering" "ordering?") 
                              ("is_pay_per_time" "ppt?") 
                              ("is_published" "publshd?") 
                              ("is_special_ppv" "sppv?") 
                              ("nominal_price" "price") 
                              ("pay_mode" "pay_mode") 
                              ("preview_time" "preview_time") 
                              ("rental_duration" "rental") 
                              ("sale_end_date_gmt" "sale_end_date") 
                              ("sale_start_date_gmt" "sale_start_date") 
                              ("selling_mode" "sell_md") 
                              ("validity_end_date_gmt" "validity_end_date") 
                              ("validty_start_date_gmt" "validty_start_date") 
                              ("version" "version") 
                              )) *op:sql-map-useful-columns*)




(push '("hdt3_syslog_audit" . (
                               ("action" "action")
                               ("entity" "entity")
                               ("host" "host")
                               ("identifier" "identifier")
                               ("log_time" "log_time" "date-time")
                               ("primary_key" "primary_key")
                               ("username" "username")
                               )) *op:sql-map-useful-columns*)


(push '("dt3_sys_device" . (
                            ("active" "active")
                            ("class" "class")
                            ("ip_address" "ip_address")
                            ("is_main" "is_main")
                            ("main_id" "main_id")
                            ("status" "status")
                            ("type" "type")
                            ("version" "version")
                            )) *op:sql-map-useful-columns*)
