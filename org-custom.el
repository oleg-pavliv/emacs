(require 'org)

(when (getenv "NAGRA")
  (load-library "find-lisp")
  (setq org-agenda-files (reverse (remove-if #'(lambda (file) (or (string-match "-archive\.org$" file) (string-match "-info\.org$" file))) (find-lisp-find-files (getenv "ORG-STORAGE") "\.org$"))))

  (defadvice kill-buffer (after kill-buffer-org-mode activate)
    "kill all org files when killing the *Org Agenda*"
    (let ((buf-to-kill buffer-or-name))
      (if (null buf-to-kill) (setq buf-to-kill (buffer-name)))
      (unless (stringp buf-to-kill) (setq buf-to-kill (buffer-name buf-to-kill)))
      (when (equal "*Org Agenda*" buf-to-kill)
        (dolist (buf (buffer-list))
          (when (and (string-match "\.org$" (buffer-name buf)) (string-match (getenv "ORG-STORAGE") (buffer-file-name buf)))
            (kill-buffer buf))))))

  )


(setq org-agenda-custom-commands
      `(
        ("a" "all tasks"
         ((org-agenda-list))
         ((org-agenda-remove-tags t)))
        ;; ("o" "tasks for oleg"
        ;;  ((org-agenda-list)
        ;;   (org-agenda-filter-apply ,(list "+oleg")))
        ;;  ((org-agenda-remove-tags t)))
        ;; ("d" "delegated tasks"
        ;;  ((org-agenda-list)
        ;;   (org-agenda-filter-apply ,(list "+delegated")))
        ;;  ((org-agenda-remove-tags t)))
        ("ki" "tasks for minh" tags "+Owner=\"tdi\"" nil)
        ("kr" "tasks for didier" tags "+Owner=\"dvr\"" nil)
        ("ky" "tasks for jean-yves" tags "+Owner=\"jym\"" nil)
        ))


(defun op-i:org-new-todo-item (&optional deadline) 
  (interactive "P")
  (org-todo)
  (org-schedule nil (current-time))
  (when deadline (org-deadline nil (current-time)))
  (org-priority ?A))


(define-key org-mode-map (kbd "C-<tab>") 'bubble-buffer-next)
(define-key org-mode-map (kbd "C-S-<tab>") 'bubble-buffer-previous)
(define-key org-mode-map (kbd "C-c C-n") 'op-i:org-new-todo-item)
(define-key org-mode-map (kbd "C-c 0") (lambda () 
                                         (interactive)
                                         (org-show-hidden-entry)
                                         (move-end-of-line nil)
                                         (insert " ==========> ")))

(define-key org-mode-map (kbd "C-c i") (lambda ()
                                         (interactive)
                                         (op:http-chrome (concat "IST_URL/cgi-bin/WebObjects/ist.woa/wa/inspectRequest?requestNo=" (thing-at-point 'word)))))



(defadvice org-agenda-list (after org-agenda-list-1buf activate)
  (delete-other-windows))


(defadvice org-open-at-point (around org-open-at-point-in-emacs activate)
  (ad-set-arg 0 t)
  ad-do-it
  (delete-other-windows))


(setq org-link-abbrev-alist
      '(
        ("so" . "http://stackoverflow.com/questions/")
        ))



(org-add-link-type "ie-http" 'op:http-ie)
(org-add-link-type "ff-http" 'op:http-ff)
(org-add-link-type "xml-spy" (lambda (file) (shell-command (concat "'C:/Program Files (x86)/Altova/XMLSpy2008/XMLSpy.exe' '" file "' &"))))
(org-add-link-type "w32-shell" 'op:w32-shell-open)

(org-add-link-type "load-xml-dir" (lambda (dir-buf-cust) 
                                    (let ((dbc (split-string dir-buf-cust ";")))
                                      (op:load-xml-dirs (first dbc) (second dbc) (third dbc)))))

;;use the following format: common_directory_path;relative_path1;relative_path2;...
(org-add-link-type "multi-find-file" (lambda (prefix-files) 
                                       (let ((pf (split-string prefix-files ";")))
                                         (mapcar (lambda (file)
                                                   (let ((full-path (concat (first pf) "/" file)))
                                                     (if (file-directory-p full-path)
                                                         (dired full-path "-lhAR")
                                                       (find-file full-path))
                                                     (op:mark-buffer-as-permanent)))
                                                 (cdr pf)))))

(org-add-link-type "dired-R" (lambda (dir) (dired dir "-lhAR")))
