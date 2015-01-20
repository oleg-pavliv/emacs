(defun op-i:sunrise (&optional arg)
  (interactive "P")
  (let* ((dir1) (dir2) (reg-content (get-register ?§))
         (dirs (if (stringp reg-content) (split-string reg-content ";"))))
    (if dirs
        (setq dir1 (first dirs) dir2 (second dirs))
      (dolist (buf (buffer-list)) ;; otherwise use two recent dired buffers
        (when (equal 'dired-mode (op:buffer-mode buf))
          (if (null dir1)
              (save-excursion
                (switch-to-buffer buf)
                (if (file-exists-p (dired-current-directory)) 
                    (setq dir1 (dired-current-directory))))
            (if (null dir2)
                (save-excursion
                  (switch-to-buffer buf)
                  (if (file-exists-p (dired-current-directory))
                      (setq dir2 (dired-current-directory)))))))))
    (sunrise dir1 dir2)))

(define-key (current-global-map) [f12] 'op-i:sunrise)

(defun op:sunrise-config-to-register ()
  (interactive)
  (let ((dir1) (dir2))
    (save-excursion
      (when (window-live-p sr-left-window)
        (set-buffer (window-buffer sr-left-window))
        (when (memq major-mode '(sr-mode sr-tree-mode))
          (setq dir1 default-directory)))
      (when (window-live-p sr-right-window)
        (set-buffer (window-buffer sr-right-window))
        (when (memq major-mode '(sr-mode sr-tree-mode))
          (setq dir2 default-directory))))
    (when (and dir1 dir2)
      (set-register ?§ (concat dir1 ";" dir2)))))

(define-key ctl-x-r-map [f11] 'op:sunrise-config-to-register)

(defun op-i:kill-sunrise-buffers ()
  (interactive)
  (sr-quit)
  (mapc (lambda (buffer)
          (when (equal 'sr-mode (op:buffer-mode buffer))
            (save-excursion
              (switch-to-buffer buffer)
              (dired-mode)
              ;; (let ((dir (dired-current-directory)))
              ;;   ;; (kill-buffer buffer)
              ;;   (dired-mode))
              ))) 
        (buffer-list))
  (delete-other-windows))

(define-key sr-mode-map (kbd "Q") 'op-i:kill-sunrise-buffers)


;; desactivated becase is called from within sunrise (e.g. when visiting a file)
;; (add-hook 'sr-quit-hook 'op:kill-sunrise-buffers)


(defadvice sr-goto-dir (after sr-goto-dir-goto-first-file activate)
  (dired-next-line 4))
