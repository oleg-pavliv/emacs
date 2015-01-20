(require 'css-mode)
(require 'nxml-mode)

(setq nxml-slash-auto-complete-flag t)

(add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt '("xml" "xsd" "xslt" "xsl" "html" "htm" "wsdl" "xml.template" "xhtml" "jsp") t) "\\'") 'nxml-mode))
(define-key nxml-mode-map (kbd "C-M-n") (lambda ()
                                            (interactive)
                                            (re-search-backward "<" nil t)
                                            (nxml-forward-element)
                                            ))
(define-key nxml-mode-map (kbd "C-M-p") (lambda ()
                                            (interactive)
                                            (re-search-forward ">" nil t)
                                            (nxml-backward-element)
                                            ))
;; continous validation causes memory overflow
(setq rng-nxml-auto-validate-flag nil)
;; (setq nxml-sexp-element-flag nil)


;; (add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt '("xml" "xsd" "xslt" "xsl" "html" "htm" "wsdl" "xml.template" "xhtml" "jsp") t) "\\'") 'sgml-mode))
;; (add-hook 'sgml-mode-hook
;;           '(lambda ()
;;              (sgml-electric-tag-pair-mode)))


(defun op:html-remove-empty-class-id (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "[[:blank:]]*\\(class=\"\"\\|id=\"\"\\)" nil t)
        (replace-match ""))
      (widen))))




(defun sgml-delete-tagged-text ()
  "delete text between the tags that contain the current point"
  (interactive)
  (let ((b (point)))
    (sgml-skip-tag-backward 1)
    (when (not (eq b (point)))
      ;; moved somewhere, should be at front of a tag now
      (save-excursion 
        (forward-sexp 1)
        (setq b (point)))
      (sgml-skip-tag-forward 1)
      (backward-sexp 1)
      (delete-region b (point)))))


;;------------------------------------------from http://xahlee.org/emacs/emacs_html.html  -----------------------------------

(defun op:replace-string-pairs-region (start end mylist)
  "Replace string pairs in region."
  (save-restriction 
    (narrow-to-region start end)
    (mapc
     (lambda (arg)
       (goto-char (point-min)) 
       (while (search-forward (car arg) nil t) 
         (replace-match (cadr arg)))) 
     mylist)))

(defun op:html-encode-tags (start end)
"Replace '<' by '&lt;' and other similar HTML chars that needs to be encoded. 
You may put the whole region inside <pre></pre> tags to preserve whitespaces"
  (interactive "r")
  (op:replace-string-pairs-region start end '(
                                           ("&" "&amp;")
                                           ("<" "&lt;")
                                           (">" "&gt;")
                                           ;; ("\n" "<br/>\n") 
                                           )
                               ))
;; (let ((colorized (list 
;;                   "#[abcdef[:digit:]]\\{1,6\\}[^-_0-9a-zA-z]"
;;                   (list 0 (put-text-property
;;                       (match-beginning 0)
;;                       (match-end 0)
;;                       'face (list :background (match-string-no-properties 0)))))))
;;   (setq css-font-lock-keywords (push colorized css-font-lock-keywords)))


;;-----------------------------------------------------------------------------------------------------------------------------

;; (require 'align)

;; (defcustom align-nxml-modes '(nxml-mode)
;;   "A list of modes whose syntax resembles nxml"
;;   :type '(repeat symbol)
;;   :group 'align)

;; (add-to-list 'align-rules-list
;;      '(nxml-attr
;;         (regexp  . "\\(\\s-*\\)[a-zA-z0-9]+=")
;;         (group   . 1)
;;         (modes   . align-nxml-modes)
;;         (repeat  . t)))



(provide 'nxml-custom)
