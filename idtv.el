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

(provide 'idtv)


