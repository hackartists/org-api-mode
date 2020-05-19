;;; ob-api.el --- http request in org-mode babel

;; Copyright (C) 2020 Jongseok Choi

;;; Requirements:
;; restclient.el

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'restclient)

(defvar org-babel-default-header-args:api
  `((:results . "raw"))
  "Default arguments for evaluating a restclient block.")

;;;###autoload
(defun org-babel-execute:api (body params)
  "Execute a block of Restclient code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Restclient source code block")
  (with-temp-buffer
    (let ((results-buffer (current-buffer))
          (restclient-same-buffer-response t)
          (restclient-same-buffer-response-name (buffer-name))
          (display-buffer-alist
           (cons
            '("\\*temp\\*" display-buffer-no-window (allow-no-window . t))
            display-buffer-alist)))

      (insert (buffer-name))
      (with-temp-buffer
        (dolist (p params)
          (let ((key (car p))
                (value (cdr p)))
            (when (eql key :var)
              (insert (format ":%s = <<\n%s\n#\n" (car value) (cdr value))))))
        (insert body)
	(goto-char (point-min))
	(delete-trailing-whitespace)
	(goto-char (point-min))
      (restclient-http-parse-current-and-do
       'restclient-http-do (org-babel-api-raw-payload-p params) t))

      (while restclient-within-call
        (sleep-for 0.05))

      (goto-char (point-min))
      (when (search-forward (buffer-name) nil t)
        (error "Restclient encountered an error"))

      (if (org-babel-api-return-pure-payload-result-p params)
          (org-babel-api-pure-payload-result)
        (org-babel-api-wrap-result)))))

(defun org-babel-api-wrap-result ()
  "Wrap the contents of the buffer in an `org-mode' src block."
  (let ((mode-name (substring (symbol-name major-mode)
                              0
                              -5)))
    (insert (format "#+BEGIN_SRC %s\n" mode-name))
    (goto-char (point-max))
    (insert "#+END_SRC\n")
    (buffer-string)))

(defun org-babel-api-pure-payload-result ()
  "Just return the payload."
  (let ((comments-start
         (save-excursion
           (goto-char (point-max))
           (while (comment-only-p (line-beginning-position) (line-end-position))
             (forward-line -1))
           ;; Include the last line as well
           (forward-line)
           (point))))
    (buffer-substring (point-min) comments-start)))


(defun org-babel-api-return-pure-payload-result-p (params)
  "Return `t' if the `:results' key in PARAMS contains `value' or `table'."
  (let ((result-type (cdr (assoc :results params))))
    (when result-type
      (string-match "value\\|table" result-type))))

(defun org-babel-api-raw-payload-p (params)
  "Return t if the `:results' key in PARAMS contain `file'."
  (let ((result-type (cdr (assoc :results params))))
    (when result-type
      (string-match "file" result-type))))

(provide 'ob-api)
;;; ob-api.el ends here
