(load "dired-x")
(load "dired")
(load "arc-mode")

(require 'misc)
(require 'cl)

(defvar dired-sort-map (make-sparse-keymap))

(set-register ?7 "find . -type f -printf '%TY-%Tm-%Td %Tk:%TM %9s \"%p\"\\n' | sort -g")

(defun concat-string-list (list) 
  "Return a string which is a concatenation of all elements of the list separated by spaces" 
  (mapconcat '(lambda (obj) (format "%s" obj)) list " "))

(defun dired-zip-files (zip-file)
  "Create an archive containing the marked files."
  (interactive "sEnter name of zip file: ")
  (let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
    (shell-command (concat "7z a " zip-file " "
                           (concat-string-list 
                            (mapcar
                             '(lambda (filename) (file-name-nondirectory filename))
                             (dired-get-marked-files))))))

  (revert-buffer))


(define-key dired-mode-map "z" 'dired-zip-files)


(defun op:dired-get-next-marked-file ()
  (dired-prev-marked-file 1 t)
  (dired-get-filename))



(defun op-i:ido-find-file ()
  "find a file use regex match as you type a name"
  (interactive)
  (if (equal 'dired-mode (op:buffer-mode))
      (let ((move-ok t))
        (save-excursion
          (while (and move-ok (not (dired-get-filename nil t))) 
            (if (not (eq 0 (forward-line)))
                (setq move-ok nil))) ;; forward-line no more possible
          (unless move-ok ;; we are at the buffer's end no file found, let's move backward
            (setq move-ok t)
            (while (and move-ok (not (dired-get-filename nil t))) 
              (if (not (eq 0 (forward-line -1)))
                  (setq move-ok nil))))
          (ido-file-internal ido-default-file-method nil (if move-ok (file-name-directory (dired-get-filename)) nil))))
    (ido-file-internal ido-default-file-method)))

(define-key (current-global-map) (kbd "C-x C-f") 'op-i:ido-find-file)
(define-key (current-global-map) (kbd "C-x f") 'op-i:ido-find-file)


(define-key dired-mode-map "=" (lambda () (interactive) 
                                 (ediff-files (op:dired-get-next-marked-file) (op:dired-get-next-marked-file))))


(define-key dired-mode-map "c" 'op:shell)

(define-key dired-mode-map "s" dired-sort-map)

(define-key dired-sort-map "s" (lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
(define-key dired-sort-map "x" (lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
(define-key dired-sort-map "t" (lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
(define-key dired-sort-map "n" (lambda () "sort by Name" (interactive) (dired-sort-other dired-listing-switches)))


(defun op-i:dired (rec)
  "customized dired: will display directory recursively when called with an argument"
  (interactive "P")
  (let ((dir (car (find-file-read-args "Dired: " nil))) 
        ;; h 'human readable' option causes problem while copying files: it raises an error 'No file on this line'
        (opts (if rec (read-string "options: " "-lAR") "-lA")))
    (if (file-directory-p dir) (dired dir opts))))

(define-key (current-global-map) (kbd "C-x C-d") 'op-i:dired)



(defun op:shell (&optional arg)
  "shart a shell. Rename a shell buffer by adding to the directory a suffix 'shell'"
  (interactive "P")
  (let* ((cd (car (last (split-string default-directory "[/\\]+" t)))) 
         (name (concat cd "-shell")) 
         (cur-buf (current-buffer)))
    (if (get-buffer name)
        (if (and (equal 'shell-mode (op:buffer-mode name)) (equal default-directory (op:buffer-dir name)))
            (switch-to-buffer name)
          (shell))
      (progn 
        (shell name)
        (buffer-disable-undo (get-buffer name))
        (delete-other-windows)
        (if (and arg (equal'dired-mode (op:buffer-mode cur-buf)))
            (kill-buffer cur-buf))))))


(defun op-i:dired-copy-region-as-kill (arg)
  "Put a file name in the kill ring. With arg C-u copies file in unix format. With C-u C-u copies file in windows format (backslash)"
  (interactive "P")
  (let* ((file (dired-get-filename 'no-dir t)) (full-file (concat (dired-current-directory) file)))
    (if (and arg file)
        (save-excursion
          (push-mark-command nil nil)
          (kill-new (cond
                     ((equal (car arg) 4) full-file)
                     ((equal (car arg) 16) (replace-regexp-in-string "/" "\\\\" full-file)))))
      (if (and (mark t) (point)) (kill-ring-save (mark t) (point))))))

(define-key dired-mode-map (kbd "M-w") 'op-i:dired-copy-region-as-kill)


(defun op:http-ie (address)
  "open address in the internet explorer. copy address replacing slashes with backslashes because ie requires backslashes in some addresses"
  (kill-new (replace-regexp-in-string "/" "\\\\" address))
  (start-process-shell-command "ie" nil "\"C:/Program Files/Internet Explorer/iexplore.exe\"" address))

(defun op:http-ff (address)
  "open address in the firefox"
  ;;copy address replacing slashes with backslashes because ie requires backslashes in some addresses.
  ;;do we need this for FF?
  ;; (kill-new (replace-regexp-in-string "/" "\\\\" address)) 
  (start-process-shell-command "ff" nil "\"c:/Program Files (x86)/Mozilla Firefox/firefox.exe\"" address))

(defun op:http-chrome (address)
  "open address in the chrome"
  ;;copy address replacing slashes with backslashes because ie requires backslashes in some addresses.
  ;;do we need this for chrome?
  ;; (kill-new (replace-regexp-in-string "/" "\\\\" address)) 
  (start-process-shell-command "chrome" nil "C:/Users/opavliv/AppData/Local/Google/Chrome/Application/chrome.exe" (replace-regexp-in-string " " "%20" address)))



(defun op:w32-shell-open (file)
  (w32-shell-execute "open" (concat "\"" (dired-replace-in-string "/" "\\" file) "\"")))


(defun op:process-file-by-ext (file &optional arg)
  (let* ((ext (file-name-extension file)) (name (file-name-sans-extension (file-name-nondirectory file))))
    (cond 
     ((equal ext "dot")
      (unless (file-exists-p (concat file ".jpg"))
        (shell-command (concat (getenv "GRAPHVIZ_HOME") "/bin/dot.exe -Gratio=0.7 -Tjpg '" file "' -o '" file ".jpg'")))
      (find-file (concat file ".jpg")))
     ((or (equal ext "dll") (equal ext "exe"))
      (op:get-imports-exports file "imp-exp")
      (switch-to-buffer "imp-exp"))
     ;; ((equal ext "class")
     ;;  (shell-command (concat (getenv "utils") "/jd-gui.exe \"" file "\"")))
     ((equal ext "class")
      (let ((name-java (concat name ".java")))
        (op:decompile-java-class file name-java)
        (switch-to-buffer name-java)
        (java-mode)))
     ((equal ext "xsd")
      (shell-command (concat "'C:/Program Files (x86)/Altova/XMLSpy2008/XMLSpy.exe' '" file "' &") (get-buffer-create (time-stamp-string))))
     ;; ((equal ext "mtdat")
     ;;  (op:dired-plot-dat-file))
     ;; ((string-match "health-check\\.log" file 0) 
     ;;  (op:dired-plot-health-check arg))
     ((or (equal ext "html") (equal ext "htm"))
      (op:http-ie file))
     (t (find-file-other-window file)))  ;;(replace-regexp-in-string "/" "\\\\" file) 
    (delete-other-windows)))



(defun op-i:process-file-by-ext-alt(&optional arg)
  (interactive "P")
  (let* ((dir default-directory) 
         (file (dired-get-file-for-visit))
         (ext (file-name-extension file))
         (name (file-name-sans-extension (file-name-nondirectory file)))
         )
    (cond 
     ((equal ext "class")
      (cond
       ((equal arg '(4))
        (let ((j-buf (get-buffer-create (concat name ".j"))))
          (shell-command (concat "java JasminifierClassAdapter \"" file "\"") j-buf)
          (switch-to-buffer j-buf)))
       (t
        (let ((name-jdec (concat name ".jdec"))
              (jdec-dir (concat (getenv "utils") "\\jdec\\")))
          (setq default-directory jdec-dir)
          (shell-command (concat "java -cp \".;" jdec-dir "jdec.jar\" jdec.decompiler.main.JDec -option dc -outputFolder " (getenv "temp") "/jdec-decompile -showImports true -input \"" file "\""))
          ;; (shell-command (concat "java -cp \".;" jdec-dir "jdec20.jar\" net.sf.jdec.main.ConsoleLauncher -option dc -outputFolder " (getenv "temp") "/jdec-decompile -showImports true -input \"" file "\""))
          (setq default-directory dir)
          (cd dir)
          (setq dec-file (eshell-command-result (concat "find " (getenv "temp") "/jdec-decompile/ -name \"" name-jdec "\"" )))
          (string-match "\x0a" dec-file)
          (setq dec-file (replace-match "" t nil dec-file))
          (find-file dec-file)
          (java-mode)))
       )
      )
     (t (find-file-other-window file)))
    (delete-other-windows)))

(defun op:dired-find-file-literally ()
  (let* ((file (dired-get-file-for-visit)) (buf (get-buffer-create (concat file ".literal"))))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      (insert-file-contents-literally file))
    (switch-to-buffer buf)))

;;(define-key dired-mode-map "\M-m" 'dired-find-file-other-window-ext)
(define-key dired-mode-map "\M-m" (lambda (arg) (interactive "P")
                                    (op:process-file-by-ext (dired-get-file-for-visit) arg)))
;; (define-key dired-mode-map "\M-M" (lambda (arg) (interactive "P") (op:dired-find-file-other-window-shift-ext arg)))
(define-key dired-mode-map "\M-M" 'op-i:process-file-by-ext-alt)
(define-key dired-mode-map "\M-p" 'dired-prev-marked-file)
(define-key dired-mode-map "\M-n" 'dired-next-marked-file)
(define-key dired-mode-map (kbd "C-é") 'kill-buffer-quitly)
(define-key dired-mode-map (kbd "M-é") 'kill-buffer-other-window)
(define-key dired-mode-map (kbd "C-ö") 'kill-buffer-max)

(define-key dired-mode-map [(meta return)] (lambda() "dired directory recursively" (interactive)
                                             (if (file-directory-p (dired-get-file-for-visit)) (dired (dired-get-file-for-visit) "-lhAR"))))
(define-key dired-mode-map (kbd "M-l") (lambda () (interactive) (op:dired-find-file-literally)))
(define-key dired-mode-map (kbd "M-i") (lambda()  (interactive)
                                         (op:dired-find-file-literally)
                                         (setq buffer-undo-list nil)
                                         (hexl-mode)))



;;------------------------------------------------ xsd to rgc -----------------------------------------------------------------------
(defun op:xsd-validate (xsd-file)
  (interactive "fEnter xsd file: ")
  (let* ((file (dired-get-file-for-visit))
         (dir (file-name-directory file))
         (fn (file-name-nondirectory file))
         (buf (get-buffer-create (concat (file-name-sans-extension file) "-validate")))
         (cmd "java -Xms128m -Xmx1024m -Xss512k xv ")
         (xerces (getenv "XERCES_HOME")))
    (setenv "CLASSPATH" (concat (getenv "home") "/emacs/bin;" xerces  "/serializer.jar;" xerces "/xercesImpl.jar;" xerces "xml-apis.jar;"))
    (shell-command (concat "java -Xms128m -Xmx1024m -Xss512k XmlValidate \"" file "\"  \"" xsd-file "\" \"" dir "\" \"" fn "\"") buf)
    (switch-to-buffer buf)
    (grep-mode)))


(defun op:xsl-transform (&optional xml-file xsl-file output-file)
  (interactive)
  (let* ((file1 (or xml-file (op:dired-get-next-marked-file)))
         (file2 (or xsl-file (op:dired-get-next-marked-file)))
         (xsl (if (string-match "\\.xslt?$" file1) 
                  file1
                (if (string-match "\\.xslt?$" file2) file2 nil)))
         (xml (if (equal xsl file1) file2 file1))
         (cmd "java -Xms128m -Xmx1024m -Xss512k org.apache.xalan.xslt.Process -IN \"%s\" -XSL \"%s\" -OUT \"%s\""))
    (when (and xsl xml)
      (unless output-file
        (setq output-file (concat (file-name-directory xml) "tranformed-" (file-name-nondirectory xml))))
      (shell-command (format cmd xml xsl output-file)))))

;; (defun op:xsl-indent ()
;;   (interactive)
;;   (op:xsl-transform (dired-get-file-for-visit) (concat (getenv "home") "/emacs/xsl/indent.xsl")))

(defun op:xml-indent ()
  (interactive)
  (let* ((in (dired-get-file-for-visit))
         (out (concat (file-name-directory in) "fmt-" (file-name-nondirectory in)))
         (tidy-dir (concat (getenv "utils") "/jtidy-r938/")))
    (shell-command (concat "java -jar " tidy-dir "jtidy-r938.jar -config " tidy-dir "jtidy.properties " in " > " out))
    ))
  


(defun op:ant-visualize-dependecies (neato)
  (interactive "P")
  (let* ((in-file (dired-get-file-for-visit))
         (dot-file (concat in-file ".dot"))
         (jpg-file (concat in-file ".jpg"))
         (grvz-opts (if neato "-Gmodel=subset -Glayout=neato -Goverlap=false -Gsplines=true -Gpack=true -Gsep=0.1" "-Gratio=0.7")))
    (if (equal "xml" (file-name-extension in-file))
        (progn
          (op:xsl-transform in-file (concat (getenv "home") "/emacs/xsl/ant2dot.xsl") dot-file)
          (shell-command (concat (getenv "GRAPHVIZ_HOME") "/bin/dot.exe " grvz-opts " -Tjpg -o " jpg-file " " dot-file))
          (delete-file dot-file)
          ;; (find-file jpg-file)x
          )
      (beep))))



;;------------------------------------------------ doc to txt -----------------------------------------------------------------------
(defun op:doc-to-txt (use-msword &optional doc-file)
  (interactive "P")
  (let ((doc (or doc-file (and (equal major-mode 'dired-mode) (dired-get-file-for-visit)))))
    (if use-msword (shell-command (concat "\"c:/Program Files (x86)/Microsoft Office/Office14/WINWORD.EXE\" \"" doc "\" /mSaveAsText /q /n"))
      (shell-command (concat (getenv "utils") "/Text-Mining-Tool/minetext.exe '" doc "' '" doc ".txt'")))))


;;---------------------------------------------------------------------------------------------------------------------------------------

(define-key dired-mode-map (kbd "C-$") (lambda ()
                                         (interactive)
                                         (move-beginning-of-line 1)
                                         (when (re-search-forward "^[[:blank:]]*\\([a-zA-Z]:.*?\\):\\(.*?\\)$" nil t)
                                           (let ((curr-dir (match-string-no-properties 1)) (curr-line) (continue t))
                                             (move-beginning-of-line 1)
                                             (while (and continue (re-search-forward (concat "^[[:blank:]]*" curr-dir "\\(.*?\\):\\(.*?\\)$") nil t))
                                               (setq curr-line (count-lines 1 (point)))
                                               (dired-hide-subdir 1)
                                               (setq continue (not (equal curr-line (count-lines 1 (point)))))
                                               (move-beginning-of-line 1))))))


(setq ls-lisp-verbosity nil)

(setq dired-listing-switches "-lhA")


;; redefine the emacs distribution, commenting out adding * to the executable
(defun ls-lisp-classify (filedata)
  (let ((file-name (car filedata))
        (type (cadr filedata)))
    (cond (type
           (cons
            (concat file-name (if (eq type t) "/" "@"))
            (cdr filedata)))
          ;; comment out because of bug dired visit
          ;;	  ((string-match "x" (nth 9 filedata))
          ;; 	   (cons
          ;; 	    (concat file-name "*")
          ;; 	    (cdr filedata)))
          (t filedata))))

(add-to-list 'auto-mode-alist '("\\.[rs]ar\\'" . archive-mode))

(defun is-archive-ext(name)
  (or (string-match "\\.zip$" name) (string-match "\\.jar$" name) (string-match "\\.ear$" name) (string-match "\\.aar$" name) (string-match "\\.war$" name) (string-match "\\.sar$" name) (string-match "\\.rar$" name)))

(when (string-equal "windows-nt" system-type)

  (defadvice archive-zip-extract (before archive-zip-extract-before (archive name))
    (my-archive-extract-to-stdout archive name archive-zip-extract))

  (defadvice archive-zip-extract (around archive-zip-extract-around (archive name))
    (if (is-archive-ext name) ad-do-it))


  (ad-activate 'archive-zip-extract)

  (defun my-archive-extract-to-stdout (archive name command)
    (let* ((7z-temp (concat (getenv "temp") "/7ziptemp")) 
           (7z-info (concat (getenv "temp") "/7zipinfo"))
           (extracted-file (concat 7z-temp "/" name)))
      (message (concat "my-archive-extract-to-stdout " archive ", name=" name ", command="(car command)))
      (cond
       ((string-match "\\.class$" name)
        ;; (if (file-exists-p 7z-temp) (dired-delete-file 7z-temp 'always)) ;; commented out to be able to decompile inner class
        (unless (file-exists-p 7z-temp) (make-directory 7z-temp))
        (if (file-exists-p extracted-file) (delete-file extracted-file))
        (call-process (car command) nil "7zipinfo" nil  "x" (concat "-o" 7z-temp) archive name)
        (op:process-file-by-ext extracted-file)
        (goto-char (point-min)))
       ((not (is-archive-ext name))
        (call-process (car command) nil (list name 7z-info) nil "x" "-so" archive name)
        (switch-to-buffer name)
        (goto-char (point-min))
        (goto-char (point-min))
        (set-auto-mode)))))
  )


(setq dired-recursive-copies 'always)

(when (string-equal "windows-nt" system-type)
  (eval-after-load "dired" '(define-key dired-mode-map "X" (lambda () (interactive) (op:w32-shell-open (dired-get-filename)))))
  (eval-after-load "dired" '(define-key dired-mode-map "W" (lambda () (interactive) (op:w32-shell-open (dired-current-directory)))))
  )


(defun op:list-files-sorted-date (arg)
  (interactive "P")
  (if (equal 'dired-mode (op:buffer-mode))
      (let ((old-max comint-buffer-maximum-size))
        (setq comint-buffer-maximum-size 10000000)
        (op:send-cmd-to-shell (concat "find . -type f " (if arg " -mtime -1") " -printf '%TY-%Tm-%Td %TT \"%p\"\\n' | sort\n") (concat (dired-current-directory) "-sorted"))
        (setq comint-buffer-maximum-size old-max)
        (delete-other-windows))
    (message "you can call this command only from the dired buffer")))


(defun op:list-files-sorted-size (arg)
  (interactive "P")
  (if (equal 'dired-mode (op:buffer-mode))
      (progn
        (op:send-cmd-to-shell (concat "find . -type f " (if arg (concat "-size +" (number-to-string arg) "k")) " -printf '%9k \"%p\"\\n' | sort -g \n") (concat (dired-current-directory) "-sorted"))
        (delete-other-windows))
    (message "you can call this command only from the dired buffer")))




(define-key dired-mode-map (kbd "C-c g")  'magit-status)



