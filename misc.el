(require 'grep)
(require 'artist)
(require 'calendar)


(eval-when-compile (require 'cl))

;; copy the matched string while doing the search
(define-key isearch-mode-map (kbd "M-w") (lambda () (interactive) (copy-region-as-kill isearch-other-end (point))))


(define-key calendar-mode-map (kbd "M-n") 'calendar-forward-month)
(define-key calendar-mode-map (kbd "M-p") 'calendar-backward-month)




(defun op:insert-dates ()
  "insert a list of 100 dates"
  (interactive)
  (let* ((date (org-read-date))
         (day (nth 3 (parse-time-string date)))
         (month (nth 4 (parse-time-string date)))
         (year (nth 5 (parse-time-string date)))
         (time (encode-time 1 1 0 day month year)))
    (dotimes (n 100)
      (insert (format-time-string "%D %a, " time))
      (setq day (1+ day))
      (setq time (encode-time 1 1 0 day month year)))))


;; (set-frame-parameter nil 'alpha '(100 100))
(defun toggle-transparency (unused)
  (interactive "P")
  (let ((curr-alpha (frame-parameter nil 'alpha)))
    (if (or (null curr-alpha) (/= (cadr curr-alpha) 100))
        (set-frame-parameter nil 'alpha '(100 100))
      (set-frame-parameter nil 'alpha '(50 50)))))


(define-key (current-global-map) (kbd "C-c t") 'toggle-transparency)


(defun op:get-buffer-match(m)
  (buffer-substring-no-properties (match-beginning m) (match-end m)))


(defun unix-file ()
      "Change the current buffer to Latin 1 with Unix line-ends."
      (interactive)
      (set-buffer-file-coding-system 'iso-latin-1-unix t))

(defun dos-file ()
      "Change the current buffer to Latin 1 with DOS line-ends."
      (interactive)
      (set-buffer-file-coding-system 'iso-latin-1-dos t))

;; (load-file (expand-file-name "~/emacs/nxml-custom.el"))
;; (require 'nxml-custom)

(load-file (expand-file-name "~/emacs/addons/bubble-buffer.el"))
(require 'bubble-buffer)

;; disabled becase emacs brings up the debugger if buffers position is nil
;; (defadvice window-configuration-to-register (after window-configuration-to-register-no-point activate)
;;   "Avoid storing current buffer's position in the register. We want to stay on the last used position, not to jump to the saved one"
;;   (set-register register (list (current-window-configuration) nil)))

(add-hook
 'java-mode-hook
 '(lambda () "Treat Java 1.5 @-style annotations as comments. Otherwise they are not indented correctly"
    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(define-key (current-global-map) (kbd "C-z") '(lambda (beg end) (interactive "r") (if mark-active (delete-region beg end))))



(define-key artist-mode-map [S-down-mouse-1] 'artist-mouse-choose-operation)


(add-hook 'occur-hook (lambda () (pop-to-buffer occur-buf)))

(add-hook 'grep-mode-hook (lambda () 
                            (pop-to-buffer (get-buffer "*grep*"))
                            (delete-other-windows)
                            (toggle-read-only 0)))


(defun op:align (beg end)
  (interactive "r")
  (align-regexp beg end "[,=]\\(\\s-*\\)" 1 1 t))


(defun op:get-clipboard()
  (with-temp-buffer (cua-paste nil) (buffer-substring-no-properties (point-min) (point-max))))


(defun delete-line (&optional arg)
  "copy-paste of kill-line, replace the kill-region with the delete-region"
  (interactive "P")
  (delete-region (point)
               (progn
                 (if arg
                     (forward-visible-line (prefix-numeric-value arg))
                   (if (eobp)
                       (signal 'end-of-buffer nil))
                   (let ((end (save-excursion (end-of-visible-line) (point))))
                     (if (or (save-excursion
                               (unless show-trailing-whitespace
                                 (skip-chars-forward " \t" end))
                               (= (point) end))
                             (and kill-whole-line (bolp)))
                         (forward-visible-line 1)
                       (goto-char end))))
                 (point))))

(define-key (current-global-map) (kbd "C-k") 'delete-line)

(defun delete-word (&optional arg)
  "copy-paste of kill-word"
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "copy-paste of backward-kill-word"
  (interactive "p")
  (delete-word (- arg)))

(define-key (current-global-map) (kbd "M-d") 'delete-word)
(define-key (current-global-map) (kbd "M-<backspace>") 'backward-delete-word)



(defun op-i:dublicate-line ()
  "copy the current line"
  (interactive)
  (let ((begin (line-beginning-position)) (end (line-end-position)))
    (move-beginning-of-line 2)
    (insert (concat  (buffer-substring-no-properties begin end) "\n"))
    (previous-line)))

(define-key (current-global-map) (kbd "M-c") 'op-i:dublicate-line)


;;------------------- taken from http://stackoverflow.com/questions/3156450/shift-a-region-or-line-in-emacs

;; (defun op:shift-text (distance)
;;   (if (use-region-p)
;;       (let ((mark (mark)))
;;         (save-excursion
;;           (indent-rigidly (region-beginning)
;;                           (region-end)
;;                           distance)
;;           (push-mark mark t t)
;;           (setq deactivate-mark nil)))
;;     (indent-rigidly (line-beginning-position)
;;                     (line-end-position)
;;                     distance)))

;; (defun op:shift-right (count)
;;   (interactive "p")
;;   (shift-text count))

;; (defun op:shift-left (count)
;;   (interactive "p")
;;   (shift-text (- count)))

;;--------------------------------------------------------------------------------



;;(add-hook 'find-file-hooks '(lambda () (when (not (file-exists-p (buffer-file-name))) (set-buffer-modified-p t))))  ;; an alternative is M-! touch filename

(defun op:buffer-mode (&optional buf)
  (let ((b (or buf (current-buffer))))
    (save-excursion (set-buffer b) major-mode)))


(defun op:buffer-dir (&optional buf)
  (let ((b (or buf (current-buffer))))
    (save-excursion (set-buffer b) default-directory)))


(defun op:str-null (s) (if (> (length s) 0) s nil))


(defun sudo-shell-cmd (cmd)
  (shell-command (concat "echo " (read-passwd "Password? ") " | sudo -S " cmd)))


(defun op:indent-region (beg end)
  (when (or (equal major-mode 'nxml-mode) (equal major-mode 'html-mode))
    (op:html-remove-empty-class-id beg end))
  (indent-region beg end))

(define-key (current-global-map) (kbd "C-!") '(lambda (beg end) (interactive "r") (op:indent-region beg end)))


(defun op-i:next-visual-line (&optional line)
  "move to the next 'line' visual lines. Useful for long wrapped lines"
  (interactive "p") 
  (setq line-move-visual t) 
  (next-line line) 
  (setq line-move-visual nil))

(define-key (current-global-map) (kbd "M-p") '(lambda (unused) (interactive "P") (op-i:next-visual-line -1)))
(define-key (current-global-map) (kbd "M-n") '(lambda (unused) (interactive "P") (op-i:next-visual-line 1)))



(defun op:send-cmd-to-shell (cmd buf)
  "run a shell, then do some extra stuff"
  (let ((shell-buf (get-buffer-create buf)))
    (shell shell-buf)
    (comint-send-string (get-buffer-process shell-buf) cmd)))

;;(op:send-cmd-to-shell "ls\ncd u:/emacs/addons\ncat eimp.el\n" "test")


(defun op:str-trim (str)
  (replace-regexp-in-string "\\(.*?\\)[[:blank:]]+$" "\\1" (replace-regexp-in-string "^[[:blank:]]+\\(.*\\)" "\\1" str)))


(defun op-i:quit-ediff-without-confirmation ()
  (interactive)
  (let ((ediff-p4 (or (string-match "*P4[[:blank:]]" (buffer-name (ediff-get-buffer 'A)))
                      (string-match "*P4[[:blank:]]" (buffer-name (ediff-get-buffer 'B))))))
    (ediff-really-quit nil)
    (delete-other-windows)
    (unless ediff-p4
      (kill-buffer-quitly)
      (kill-buffer-other-window 1))
    ))


(add-hook 'ediff-keymap-setup-hook (lambda ()
                                     (define-key ediff-mode-map (kbd "Q") 'op-i:quit-ediff-without-confirmation)
                                     ;; (define-key ediff-mode-map (kbd "l") (lambda ()
                                     ;;                                        (interactive)
                                     ;;                                        (with-current-buffer (ediff-get-buffer 'A)
                                     ;;                                          (hs-show-all))
                                     ;;                                        (with-current-buffer (ediff-get-buffer 'B)
                                     ;;                                          (hs-show-all))
                                     ;;                                        ))
                                     ))

(defun op:horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window) (- cur mid)))))


(defadvice kill-new (before kill-new-push-xselection-on-kill-ring activate)
  "Before putting new kill onto the kill-ring, add the clipboard/external selection to the kill ring"
  (let ((have-paste (and interprogram-paste-function (funcall interprogram-paste-function))))
    (when have-paste (push have-paste kill-ring))))


(defun op:winfile-to-cygfile (file)
  (replace-regexp-in-string "\\\\" "/" (replace-regexp-in-string "^\\([a-zA-Z]\\):" "/cygdrive/\\1" file)))


(defun op:replace-controlM(with)
  (interactive "sReplace with: ")
  (goto-char (point-min))
  (replace-string "" (or with "\n"))
  (set-buffer-modified-p nil)
  (goto-char (point-min)))


(define-key (current-global-map) (kbd "C-c m") (lambda (arg) (interactive "P") (op:replace-controlM (if arg "" "\n"))))



(defun op:toggle-read-only(&optional arg)
  (let ((filename buffer-file-name))
    (if (and filename (not (file-writable-p filename)))
        (revert-buffer nil t)
      (toggle-read-only arg)))
  (buffer-enable-undo))

(define-key (current-global-map) (kbd "C-x C-q") (lambda () (interactive) (op:toggle-read-only)))


(defun op:comment-xml ()
  (save-excursion
    (save-restriction
      (narrow-to-region)
      (while (re-search-forward "--" nil t)
        (when (equal "<!" 
                     (concat (string (char-before (+1 (point)))) 
                             (string (char-before (point))))
)
          (replace-match "-/-")))
      )
))

    ;; (let* ((start-rgn (min (mark t) (point)))
    ;;       (end-rgn (max (mark t) (point)))
    ;;       (rgn-first-line (line-number-at-pos start-rgn))
    ;;       (rgn-last-line (line-number-at-pos end-rgn)))
    ;;   (goto-char start-rgn)
    ;;   (beginning-of-line)
    ;;   (newline)
    ;;   (forward-line -1)
    ;;   (insert "<!--")
    ;;   (goto-char end-rgn)
    ;;   (end-of-line)
    ;;   (newline)
    ;;   (insert "-->")
    ;;   )




(defun op:comment (&optional and-copy) 
;  (interactive "*P") ;; (un)comment current line alt-;
  (unless mark-active (progn (beginning-of-line) (cua-set-mark) (end-of-line)))
  (let ((rgn (buffer-substring-no-properties (mark t) (point))))
    (unless and-copy
      (save-excursion 
        ;; (if (equal 'nxml-mode (op:buffer-mode))
        ;;     (op:comment-xml)
        ;;   (comment-dwim nil))
        (comment-dwim nil)))
    (when and-copy
      (comment-region (mark t) (point))
      (goto-char (max (mark t) (point)))
      (end-of-line)
      (newline)
      (insert rgn))))


(define-key (current-global-map) (kbd "C-c c") (lambda () (interactive) (op:comment)))
(define-key (current-global-map) (kbd "C-c w") (lambda (arg) (interactive "*P") (op:comment t)))

(define-key (current-global-map) (kbd "C-c o") 'imenu)

(define-key (current-global-map) (kbd "C-c f") 'find-function)


(define-key (current-global-map) (kbd "C-c b") 'beginning-of-buffer)
(define-key (current-global-map) (kbd "C-c e") 'end-of-buffer)
(define-key (current-global-map) (kbd "C-c d") 'ediff-buffers)



(defun op:remove-hard-wrap ()
  "Make several lines into a single long line." 
  (let ((fill-column 90002000)) 
    (fill-paragraph nil)))


(defun op:curr-line()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))


(defun op:keep-duplicate-regex(&optional re prefix)
  "keep duplicated lines (equal by a regexp)"
  (interactive "sregexp:")
  (if (string-match "\\\\(\\(.*?\\)\\\\)" re)
      (save-excursion
        (while (re-search-forward (concat "^.*?" re ".*?$") nil t)
          (let ((curr-match (op:get-buffer-match 1)) (second-found))
            (save-match-data
              (save-excursion
                (setq second-found (re-search-forward curr-match nil t))))
            (unless second-found
              (beginning-of-line)
              (kill-line)
              (kill-line)))))
    (message "regexp syntax /is a back slash:/(bla-bla-bla/)")))



(defun remove-duplicate-lines()
  "Remove duplicate lines in a buffer"
  (interactive)
  (save-excursion
    (let
        ((lines_hash (make-hash-table :test #'equal))
         (numlines (count-lines 1 (progn (end-of-buffer)(point)))))
      ;; Make a hash table with key=line 
      ;;     and value=the smallest line number that contains a line.
      (loop for i from numlines downto 1 do
            (let ((line nil))
              (goto-line i)
              (setf line (op:curr-line))
              ;; Want to store the smallest line number for 
              ;;     a particular line.
              (setf (gethash line lines_hash) i)))
      ;; If a line has a line number not equal to the smallest line, kill it.
      (loop for i from numlines downto 1 do
            (let ((line nil))
              (goto-line i)
              (setf line (op:curr-line))
              (beginning-of-line)
              (if (not (equal line ""))
                  (if (not (= 
                            (let ((min-line (gethash line lines_hash)))
                              (if (null min-line)
                                  -1
                                min-line))
                            i))
                      (kill-line 1))))))))


;;----------------------------------------------------------------------------------------------------
(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (delete-region (point)
                 (progn
                   (forward-char direction)
                   (unwind-protect
                       (search-forward (char-to-string char) nil nil arg)
                     (backward-char direction))
                   (point)))))

(define-key (current-global-map) [(meta z)] 'zap-up-to-char)


;;-------------------------------------------------- from steve yegge blog --------------------------------------------------
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil)))))) ;;

;;
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn 	
        (copy-file filename newname 1) 	
        (delete-file filename) 	
        (set-visited-file-name newname) 	
        (set-buffer-modified-p nil) 	
        t))))


;;-------------------------------------------------- show/hide code --------------------------------------------------

(defun op:hs-hide-all-comments()
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (goto-char (point-min))
     (while (re-search-forward hs-c-start-regexp nil t)
       (let ((c-reg (hs-inside-comment-p)))
         (when (and c-reg (car c-reg))
           (unless (hs-already-hidden-p) (hs-hide-comment-region (car c-reg) (cadr c-reg)))
           (goto-char (cadr c-reg))))))
   (run-hooks 'hs-hide-hook)))


(defun op:hs-hide-imports ()
  (hs-life-goes-on
   (save-excursion
     (let* ((reg-im "^[[:blank:]]*import[[:blank:]]") 
            (beg (progn (goto-char (point-min)) (re-search-forward reg-im nil t))) 
            (end (progn (goto-char (point-max)) (re-search-backward reg-im nil t) (forward-line) (point))))
       (when (and beg end (> (count-lines beg end) 1))
         (hs-hide-comment-region beg end))))))


;;-------------------------------------------------------------------------------------------------------------------------

(setq *replace-back-slash-to-forward* t)
(define-key (current-global-map) (kbd "C-c 7") (lambda (beg end)
                                (interactive "r")
                                (save-excursion
                                  (save-restriction
                                    (setq *replace-back-slash-to-forward* (not *replace-back-slash-to-forward*))
                                    (let ((from-str (if *replace-back-slash-to-forward* "/" "\\")) 
                                          (to-str (if *replace-back-slash-to-forward* "\\" "/")))
                                      (narrow-to-region beg end)
                                      (goto-char (point-min))
                                      (while (search-forward from-str nil t)
                                        (replace-match to-str nil t)))))))


(define-key (current-global-map) [f5] '(lambda (unused) (interactive "P") (revert-buffer t t t)))

;;-------------------------------------------------- buffer manipulation --------------------------------------------------

(define-key (current-global-map) [C-tab] 'bubble-buffer-next)
(define-key (current-global-map) [C-S-tab] 'bubble-buffer-previous)

(setq *permanent-buffers* '())

(defun op:mark-buffer-as-permanent(&optional buf)
  (interactive)
  (let ((b (or buf (buffer-name))))
    (message (concat "marking " b " as permanent"))
    (unless (member b *permanent-buffers*) (push b *permanent-buffers*))))


(defun op:confirm-kill-buffer(&optional buf)
  (or (not (member (or buf (buffer-name)) *permanent-buffers*)) 
      (yes-or-no-p (format "Kill buffer %s? " (buffer-name)))))


(defun kill-buffer-quitly(&optional arg)
  (interactive "P")
  (when (op:confirm-kill-buffer)
    (setq *permanent-buffers* (remove (buffer-name) *permanent-buffers*))
    (let* ((process (get-buffer-process (buffer-name))) 
           (old-val (if process (process-query-on-exit-flag process) nil)))
      (if (and process arg) (set-process-query-on-exit-flag process nil))
      (kill-buffer (buffer-name))
      (if process (set-process-query-on-exit-flag process old-val)))))


(defun op-i:maximize-other-window ()
  (interactive)
  (other-window 1)
  (delete-other-windows))


(defun kill-buffer-other-window(arg)
  "Kill the buffer in the other window, and make the current buffer full size. If no other window, kills current buffer."
  (interactive "p")
  (let ((buf (save-window-excursion
               (other-window arg)
               (current-buffer))))
    (when (op:confirm-kill-buffer (buffer-name buf))
      (delete-windows-on buf)
      (setq *permanent-buffers* (remove (buffer-name) *permanent-buffers*))
      (kill-buffer buf))))


(define-key (current-global-map) (kbd "C-é") 'kill-buffer-quitly)
(define-key (current-global-map) (kbd "C-ö") 'op-i:maximize-other-window)
(define-key (current-global-map) (kbd "M-é") 'kill-buffer-other-window)




(add-hook 'shell-mode-hook 'n-shell-mode-hook)
(defun n-shell-mode-hook ()
  "12Jan2002 - sailor, shell mode customizations."
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
  (setq comint-input-sender 'n-shell-simple-send))

(defun n-shell-simple-send (proc command)
  "17Jan02 - sailor. Various commands pre-processing before sending to shell."
  (cond
   ;; Checking for clear command and execute it.
   ((string-match "^[ \t]*clear[ \t]*$" command)
    (comint-send-string proc "\n")
    (erase-buffer))
   ;; Checking for man command and execute it.
   ((string-match "^[ \t]*man[ \t]*" command)
    (comint-send-string proc "\n")
    (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
    (setq command (replace-regexp-in-string "[ \t]+$" "" command))
    ;;(message (format "command %s command" command))
    (funcall 'man command))
   ;; Send other commands to the default handler.
   (t (comint-simple-send proc command)))
  )


;;--------------------------------------------------  --------------------------------------------------

(defun save-macro (name)                  
  "save a macro. Take a name as argument and save the last defined macro"
  (interactive "SName of the macro :")
  (kmacro-name-last-macro name)
  (find-file "~/.emacs")
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline) 
  (switch-to-buffer nil))

(defun op:apply-macro-on-region (start end command)
  (interactive "r\naCommand name (default:last keyboard macro).")
  (goto-char end)
  (let ((mark (point-marker)))
    (goto-char start)
    (while (< (point) (marker-position mark))
      (if (not (fboundp command))
          (call-last-kbd-macro)
        (command-execute command)))))

;;--------------------------------------------------  --------------------------------------------------

(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactiveq)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))


(defun swap-windows ()
  "If you have 2 windows, it swaps them." 
  (interactive)
  (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

(define-key (current-global-map) (kbd "C-c p") 'swap-windows)


(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key (current-global-map) (kbd "C-c |") 'toggle-window-split)



;------------------------------------------------------------------------------------------------------
(require 'recentf)

;; the following two lines disables loading tramp for recentf files
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(setq recentf-keep '(file-remote-p file-readable-p)) 

(recentf-mode 1)
(setq recentf-max-saved-items 500)
(define-key (current-global-map) [(meta f12)] 'recentf-open-files)


(defun op-i:ido-choose-from-recentf (arg)
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive "P")
  (let ((rc-file (ido-completing-read "Recentf open: " recentf-list nil t)))
    (find-file (if arg (file-name-directory rc-file) rc-file))))

(define-key (current-global-map) (kbd "C-x C-<") 'op-i:ido-choose-from-recentf)
;;------------------------------------------------------------------------------------------------------


;;------------------------------------------------------------------------------------------------------
;; Easy bookmarks management
;; Author: Anthony Fairchild


;;;; Keymaping examples
(define-key global-map [C-f2] 'af-bookmark-toggle)
(define-key global-map [f2] 'af-bookmark-cycle-forward)
(define-key global-map [S-f2] 'af-bookmark-cycle-reverse)
(define-key global-map [CS-f2] 'af-bookmark-clear-all)


;; Include common lisp stuff
(require 'cl)
(require 'bookmark)
(defvar af-current-bookmark nil)

(defun af-bookmark-make-name ()
  "makes a bookmark name from the buffer name and cursor position"
  (concat (buffer-name (current-buffer))
          " - " (number-to-string (point))))

(defun af-bookmark-toggle ()
  "remove a bookmark if it exists, create one if it doesnt exist"
  (interactive)
  (let ((bm-name (af-bookmark-make-name)))
    (if (bookmark-get-bookmark bm-name t)
        (progn (bookmark-delete bm-name)
               (message "bookmark removed"))
      (progn (bookmark-set bm-name)
             (setf af-current-bookmark bm-name)
             (message "bookmark set")))))


(defun af-bookmark-cycle (i)
  "Cycle through bookmarks by i.  'i' should be 1 or -1"
  (if bookmark-alist
      (progn (unless af-current-bookmark
               (setf af-current-bookmark (first (first bookmark-alist))))
             (let ((cur-bm (assoc af-current-bookmark bookmark-alist)))
               (setf af-current-bookmark
                     (if cur-bm
                         (first (nth (mod (+ i (position cur-bm bookmark-alist))
                                          (length bookmark-alist))
                                     bookmark-alist))
                       (first (first bookmark-alist))))
               (bookmark-jump af-current-bookmark)
               ;; Update the position and name of the bookmark.  We
               ;; only need to do this when the bookmark has changed
               ;; position, but lets go ahead and do it all the time
               ;; anyway.
               (bookmark-set-position af-current-bookmark (point))
               (let ((new-name (af-bookmark-make-name)))
                 (bookmark-set-name af-current-bookmark new-name)
                 (setf af-current-bookmark new-name))))
    (message "There are no bookmarks set!")))
(defun af-bookmark-cycle-forward ()
  "find the next bookmark in the bookmark-alist"
  (interactive)
  (af-bookmark-cycle 1))
(defun af-bookmark-cycle-reverse ()
  "find the next bookmark in the bookmark-alist"
  (interactive)
  (af-bookmark-cycle -1))

(defun af-bookmark-clear-all()
  "clears all bookmarks"
  (interactive)
  (setf bookmark-alist nil))



;;------------------------------------------------------------------------------------------------------
;; save undo between sessions
;; autor Trey Jackson (http://stackoverflow.com/questions/2985050/is-there-any-way-to-have-emacs-save-your-undo-history-between-sessions)

;; (defun save-undo-filename (orig-name)
;;   "given a filename return the file name in which to save the undo list"
;;   (concat (file-name-directory orig-name)
;;           "."
;;           (file-name-nondirectory orig-name)
;;           ".undo"))

;; (defun save-undo-list ()
;;   "Save the undo list to a file"
;;   (save-excursion
;;     (ignore-errors
;;       (let ((undo-to-save `(setq buffer-undo-list ',buffer-undo-list))
;;             (undo-file-name (save-undo-filename (buffer-file-name))))
;;         (find-file undo-file-name)
;;         (erase-buffer)
;;         (let (print-level
;;               print-length)
;;           (print undo-to-save (current-buffer)))
;;         (let ((write-file-hooks (remove 'save-undo-list write-file-hooks)))
;;           (save-buffer))
;;         (kill-buffer))))
;;   nil)

;; (defvar handling-undo-saving nil)

;; (defun load-undo-list ()
;;   "load the undo list if appropriate"
;;   (ignore-errors
;;     (when (and
;;            (not handling-undo-saving)
;;            (null buffer-undo-list)
;;            (file-exists-p (save-undo-filename (buffer-file-name))))
;;       (let* ((handling-undo-saving t)
;;              (undo-buffer-to-eval (find-file-noselect (save-undo-filename (buffer-file-name)))))
;;         (eval (read undo-buffer-to-eval))))))

;; (add-hook 'write-file-hooks 'save-undo-list)
;; (add-hook 'find-file-hook 'load-undo-list)


;;------------------------------------------------------------------------------------------------------


(defun op:get-clipboard-data ()
  (cond ((fboundp 'w32-get-clipboard-data) 'w32-get-clipboard-data)
        ((fboundp 'x-get-clipboard) 'x-get-clipboard)
        (t nil)))


;;---------------------------------------------------------------------- 
;; from http://stackoverflow.com/questions/6277813/unshifted-symbols-in-emacs
;; minor mode to type shifted symbols such as +"*%& as unshifted and numbers will be then shifted
;;---------------------------------------------------------------------- 
(define-minor-mode snoopy-mode
  "Toggle snoopy mode. With no argument, this command toggles the mode. Non-null prefix argument turns on the mode. Null prefix argument turns off the mode."   
  ;;   The initial value.   
  nil   
  ;; The indicator for the mode line.   
  " Snoopy"   
  ;; The minor mode bindings.   
  '(("1" . (lambda () (interactive) (insert-char ?! 1)))
    ("!" . (lambda () (interactive) (insert-char ?1 1)))     
    ;;etc 
    )) 


;;---------------------------------------------------------------------- 
;; from http://stackoverflow.com/questions/8031246/listing-all-top-level-global-variables-in-emacs
;;---------------------------------------------------------------------- 
;; (defun op:list-all-global-vars-in-emacs ()
;;   (let ((result '()))
;;     (mapatoms (lambda (x)
;;                 (when (boundp x)
;;                   (let ((file (ignore-errors
;;                                 (find-lisp-object-file-name x 'defvar))))
;;                     (when file
;;                       (push (cons x file) result))))))
;;     result))



;;---------------------------------------------------------------------- 
;; from http://trey-jackson.blogspot.com/2010/10/emacs-tip-38-automatically-diff-binary.html
;;---------------------------------------------------------------------- 
(defvar ediff-do-hexl-diff nil
  "variable used to store trigger for doing diff in hexl-mode")
(defadvice ediff-files-internal (around ediff-files-internal-for-binary-files activate)
  "catch the condition when the binary files differ

the reason for catching the error out here (when re-thrown from the inner advice)
is to let the stack continue to unwind before we start the new diff
otherwise some code in the middle of the stack expects some output that
isn't there and triggers an error"
  (let ((file-A (ad-get-arg 0))
        (file-B (ad-get-arg 1))
        ediff-do-hexl-diff)
    (condition-case err
        (progn
          ad-do-it)
      (error
       (if ediff-do-hexl-diff 
           (let ((buf-A (find-file-noselect file-A))
                 (buf-B (find-file-noselect file-B)))
             (with-current-buffer buf-A
               (hexl-mode 1))
             (with-current-buffer buf-B
               (hexl-mode 1))
             (ediff-buffers buf-A buf-B))
         (error (error-message-string err)))))))

(defadvice ediff-setup-diff-regions (around ediff-setup-diff-regions-for-binary-files activate)
  "when binary files differ, set the variable "
  (condition-case err
      (progn
        ad-do-it)
    (error
     (setq ediff-do-hexl-diff
           (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                (error-message-string err))
                (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))))
     (error (error-message-string err)))))

(provide 'misc)





;; the following code displays relative line numbers in the linum-mode
;; it can be useful with a vi compatible mode
;; it's taken from http://stackoverflow.com/questions/6874516/relative-line-numbers-in-emacs

;; (defvar my-linum-format-string "%3d")

;; (add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

;; (defun my-linum-get-format-string ()
;;   (let* ((width (1+ (length (number-to-string
;;                              (count-lines (point-min) (point-max))))))
;;          (format (concat "%" (number-to-string width) "d")))
;;     (setq my-linum-format-string format)))

;; (defvar my-linum-current-line-number 0)

;; (setq linum-format 'my-linum-relative-line-numbers)

;; (defun my-linum-relative-line-numbers (line-number)
;;   (let ((offset (- line-number my-linum-current-line-number)))
;;     (propertize (format my-linum-format-string offset) 'face 'linum)))

;; (defadvice linum-update (around my-linum-update)
;;   (let ((my-linum-current-line-number (line-number-at-pos)))
;;     ad-do-it))
;; (ad-activate 'linum-update)
