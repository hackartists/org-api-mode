;;; ob-api.el --- http request in org-mode babel

;; Copyright (C) 2020 Jongseok Choi

;;; Requirements:
;; restclient.el

;;; Code:
;; (require 'ob)
;; (require 'ob-ref)
;; (require 'ob-comint)
;; (require 'ob-eval)
;; (require 'restclient)
;; (require 'ob-api-mode)

;; (defvar org-babel-default-header-args:api
;;   `((:results . "raw")
;;     (:host . "localhost")
;;     (:port . 3200)
;;     (:scheme . "http"))
;;   "Default arguments for evaluating a restclient block.")

;; ;;;###autoload
;; (defun org-babel-execute:api (body params)
;;   "Execute a block of Restclient code with org-babel.
;; This function is called by `org-babel-execute-src-block'"
;;   (message "executing Restclient source code block")
;;   (with-temp-buffer
;;     (let ((results-buffer (current-buffer))
;;           (restclient-same-buffer-response t)
;;           (restclient-same-buffer-response-name (buffer-name))
;;           (display-buffer-alist
;;            (cons
;;             '("\\*temp\\*" display-buffer-no-window (allow-no-window . t))
;;             display-buffer-alist)))

;;       (insert (buffer-name))
;;       (with-temp-buffer
;;         (dolist (p params)
;;           (let ((key (car p))
;;                 (value (cdr p)))
;;             (when (eql key :var)
;;               (insert (format ":%s = <<\n%s\n#\n" (car value) (cdr value))))))
;;         (insert body)
;; 	(goto-char (point-min))
;; 	(delete-trailing-whitespace)
;; 	(goto-char (point-min))
;;       (restclient-http-parse-current-and-do
;;        'restclient-http-do (org-babel-api-raw-payload-p params) t))

;;       (while restclient-within-call
;;         (sleep-for 0.05))

;;       (goto-char (point-min))
;;       (when (search-forward (buffer-name) nil t)
;;         (error "Restclient encountered an error"))

;;       (if (org-babel-api-return-pure-payload-result-p params)
;;           (org-babel-api-pure-payload-result)
;;         (org-babel-api-wrap-result)))))

;; (defun org-babel-api-wrap-result ()
;;   "Wrap the contents of the buffer in an `org-mode' src block."
;;   (let ((mode-name (substring (symbol-name major-mode)
;;                               0
;;                               -5)))
;;     (insert (format "#+BEGIN_SRC %s\n" mode-name))
;;     (goto-char (point-max))
;;     (insert "#+END_SRC\n")
;;     (buffer-string)))

;; (defun org-babel-api-pure-payload-result ()
;;   "Just return the payload."
;;   (let ((comments-start
;;          (save-excursion
;;            (goto-char (point-max))
;;            (while (comment-only-p (line-beginning-position) (line-end-position))
;;              (forward-line -1))
;;            ;; Include the last line as well
;;            (forward-line)
;;            (point))))
;;     (buffer-substring (point-min) comments-start)))


;; (defun org-babel-api-return-pure-payload-result-p (params)
;;   "Return `t' if the `:results' key in PARAMS contains `value' or `table'."
;;   (let ((result-type (cdr (assoc :results params))))
;;     (when result-type
;;       (string-match "value\\|table" result-type))))

;; (defun org-babel-api-raw-payload-p (params)
;;   "Return t if the `:results' key in PARAMS contain `file'."
;;   (let ((result-type (cdr (assoc :results params))))
;;     (when result-type
;;       (string-match "file" result-type))))

;; (provide 'ob-api)
;;; ob-api.el ends here



(require 'ob)
(require 's)
(require 'subr-x)
(require 'json)
(require 'ob-api-mode)
(require 'cl-lib)

(defconst org-babel-header-args:api
  '((pretty . :any)
    (proxy . :any)
    (noproxy . :any)
    (curl . :any)
    (cookie . :any)
    (schema . :any)
    (host . :any)
    (port . :any)
    (user . :any)
    (username . :any)  ;; deprecated, use user instead
    (password . :any)  ;; deprecated
    (follow-redirect . :any)
    (path-prefix . :any)
    (resolve . :any)
    (max-time . :any))
  "http header arguments")

(defvar org-babel-default-header-args:api
  `((:results . "raw")
    (:pretty . "yes")
    (:host . "localhost")
    (:port . 80)
    (:scheme . "http"))
  "Default arguments for evaluating a restclient block.")


(defgroup ob-api nil
  "org-mode blocks for http request"
  :group 'org)

(defcustom ob-api:max-time 10
  "maximum time in seconds that you allow the whole operation to take"
  :group 'ob-api
  :type 'integer)

(defcustom ob-api:remove-cr nil
  "remove carriage return from header"
  :group 'ob-api
  :type 'boolean)

(defcustom ob-api:curl-custom-arguments nil
  "List of custom headers that shall be added to each curl request"
  :group 'ob-api
  :type '(repeat (string :format "%v")))

(cl-defstruct ob-api-request method url headers body)
(cl-defstruct ob-api-response headers body headers-map)

(defun ob-api-parse-request (input)
  (let* ((headers-body (ob-api-split-header-body input))
         (headers (s-split-up-to "\\(\r\n\\|[\n\r]\\)" (car headers-body) 1))
         (method-url (split-string (car headers) " ")))
    (make-ob-api-request
     :method (car method-url)
     :url (cadr method-url)
     :headers (if (cadr headers) (s-lines (cadr headers)))
     :body (cadr headers-body))))

(defun ob-api-parse-response (response)
  (let* ((headers-body (ob-api-split-header-body response))
         (headers-map (mapcar 'ob-api-parse-header (s-lines (car headers-body)))))
    (make-ob-api-response
     :headers (car headers-body)
     :body (cadr headers-body)
     :headers-map headers-map)))

(defun ob-api-split-header-body (input)
  (let ((splited (s-split-up-to "\\(\r\n\\|[\n\r]\\)[ \t]*\\1" input 1)))
    (if (and (string-match "^HTTP/\\(1.[0-1]\\|2\\) \\(30\\|100\\)" (car splited))
             (string-match "^HTTP/\\(1.[0-1]\\|2\\)" (cadr splited)))
        (ob-api-split-header-body (cadr splited))
      splited)))

(defun ob-api-parse-header (line)
  (let ((key-value (s-split-up-to ": " line 1)))
    `(,(s-downcase (car key-value)) . ,(cadr key-value))))

(defun ob-api-parse-content-type (content-type)
  (when content-type
    (cond
     ((string-match "json" content-type) 'json)
     ((string-match "html" content-type) 'html)
     ((string-match "xml" content-type) 'xml))))

(defun ob-api-shell-command-to-string (command input)
  (with-temp-buffer
    (insert input)
    (shell-command-on-region (point-min) (point-max) command nil 't)
    (buffer-string)))

(defun ob-api-pretty-json (str)
  (if (executable-find "jq")
      (ob-api-shell-command-to-string "jq -r ." str)
    (with-temp-buffer
      (insert str)
      (json-pretty-print-buffer)
      (buffer-string))))

(defun ob-api-pretty-xml (str)
  (cond
   ((executable-find "xml_pp") (ob-api-shell-command-to-string "xml_pp" str))
   ((executable-find "xmlstarlet") (ob-api-shell-command-to-string "xmlstarlet fo" str))
   (t str)))

(defun ob-api-pretty-html (str)
  (cond
   ((executable-find "elinks") (ob-api-shell-command-to-string "elinks -dump" str))
   ((executable-find "tidy") (ob-api-shell-command-to-string "tidy -i -raw -q 2> /dev/null" str))
   ((executable-find "pup") (ob-api-shell-command-to-string "pup -p" str))
   (t str)))

(defun ob-api-pretty (body content-type)
  (if (string= "" body)
      body
    (cl-case (ob-api-parse-content-type content-type)
      (json (ob-api-pretty-json body))
      (xml (ob-api-pretty-xml body))
      (html (ob-api-pretty-html body))
      (otherwise body))))

(defun ob-api-pretty-response (response content-type)
  (setf (ob-api-response-body response)
        (ob-api-pretty (ob-api-response-body response)
                        (if (member content-type '("yes" nil))
                            (ob-api-get-response-header response "content-type")
                          content-type))))

(defun ob-api-select (response path)
  (let ((content-type (ob-api-parse-content-type
                       (ob-api-get-response-header response "content-type")))
        (body (ob-api-response-body response)))
    (cond
     ((and (eq 'json content-type) (executable-find "jq"))
      (ob-api-shell-command-to-string (format "jq -r \"%s\"" path) body))
     ((and (eq 'html content-type) (executable-find "pup"))
      (ob-api-shell-command-to-string (format "pup -p \"%s\"" path) body))
     ((and (eq 'xml content-type) (executable-find "xmlstarlet"))
      (ob-api-shell-command-to-string (format "xmlstarlet sel -t -c '%s' | xmlstarlet fo -o" path) body))
     (t body))))

(defun org-babel-expand-body:api (body params)
  (s-format body 'ob-api-aget
            (mapcar (lambda (x) (when (eq (car x) :var) (cdr x))) params)))

(defun ob-api-get-response-header (response header)
  (cdr (assoc (s-downcase header) (ob-api-response-headers-map response))))

(defun ob-api-remove-carriage-return (response)
  (setf (ob-api-response-headers response)
        (s-join "\n" (s-lines (ob-api-response-headers response))))
  response)

(defun ob-api-flatten (l)
  (cond
   ((null l) nil)
   ((atom l) (list l))
   (t
    (append (ob-api-flatten (car l)) (ob-api-flatten (cdr l))))))

(defun ob-api-aget (key alist)
  (assoc-default (intern key) alist))

(defun ob-api-construct-url (path params)
  (if (s-starts-with? "/" path)
      (s-concat
       (format "%s://" (or (assoc-default :schema params) "http"))
       (assoc-default :host params)
       (when (assoc :port params)
             (format ":%s" (assoc-default :port params)))
       (assoc-default :path-prefix params)
       path)
    path))

(defun ob-api-file (response filename)
  (let ((body (ob-api-response-body response)))
    (with-temp-file filename
      (insert body))))

(defun ob-api-manipulate-args (args)
  "table parameters will be manipulated."
  (let* ((ret '()))
         (dolist (el args)
           (cond
            ((and (eq (car el) :var) (< 2 (safe-length (cdr el))))
             (let* ((l (cdr (cdr (cdr el)))))
               (dolist (el l)
                 (let* ((key (intern (car el)))
                        (val (nth 1 el))
                        (desc (nth 2 el))
                        )
                   (unless (eq key '##)
                     (push (append '(:var) (cons key val)) ret)
                     (push (append '(:desc) (cons key desc)) ret))))))
            (t (push el ret))))
         ret
         ))

(defun ob-api-headers-to-table (headers spaces)
  "returns table string"
  (string-join (mapcar (lambda (x)  (format "%s| %s |\n" spaces (s-replace ":" " | " x))) headers)))

(defun ob-api-body-descriptor:json (body params)
  "returns description string of request body and description"
  (let* ((ret '())
         (vars (mapcar (lambda (x) (when (eq (car x) :var) (cdr x))) params))
         (descs (mapcar (lambda (x) (when (eq (car x) :desc) (cdr x))) params))
         )
    (replace-regexp-in-string
     "\\$\\({\\([^}]+\\)}\\|[0-9]+\\)"
     (lambda (md)
       (let* ((name (substring md 2 -1))
              (desc (cdr (assoc (intern name) descs)))
              (val (cdr (assoc (intern name) vars)))
              (type (cond
                     ((numberp val) "number")
                     ((booleanp val) "boolean")
                     ((stringp val) "string")
                     (t "object"))))
         (add-to-list 'ret (format "  | %s | %s | %s |" name type desc) t)
         md))
     body)
    (string-join ret "\n")))

(defun ob-api-pretty-result (request response args body params)
  "returns result string"
  (let* ((req (ob-api-parse-request body))
         (pdescs (ob-api-body-descriptor:json body params)))
    (concat
     "- Request URL\n"
     "  " (format "| URL | %s  |\n" (ob-api-request-url req))
     "  " (format "| Method | %s |\n" (ob-api-request-method req))
     (ob-api-headers-to-table (ob-api-request-headers request) "  ")
     "\n"
     (when (not (equal "" pdescs))
       (concat
        "- Parameters\n"
        "  | Name | Type | Description |\n"
        "  |--+--+--|\n"
        pdescs "\n"
        "\n"
        ))
     "- Example\n"
     "  - Command\n"
     "    #+BEGIN_SRC shell\n"
     (format "curl %s\n" (string-join (mapcar 'shell-quote-argument (ob-api-flatten args)) " "))
     "    #+END_SRC\n"
     "\n"
     "  - Request\n"
     "    #+BEGIN_SRC js\n"
     (format "// %s %s\n" (ob-api-request-method request) (ob-api-request-url request))
     (when (ob-api-request-headers request)
       (format "// %s\n" (string-join (ob-api-request-headers request) "\n// ")))
     (when (ob-api-request-body request)
       (format "\n%s" (ob-api-pretty-json (ob-api-request-body request))))
     "    #+END_SRC\n"
     "\n"
     "  - Response\n"
     "    #+BEGIN_SRC js\n"
     (when (ob-api-response-headers response)
       (format "// %s \n" (s-replace "" "" (s-replace "\n" "\n// " (ob-api-response-headers response)))))
     "\n"
     (when (ob-api-response-body response)
       (format "%s"  (ob-api-pretty-json (ob-api-response-body response))))
     "    #+END_SRC\n")))

(defun org-babel-execute:api (body args)
  (let* ((params (ob-api-manipulate-args args)))
  (let* ((request (ob-api-parse-request (org-babel-expand-body:api body params)))
         (proxy (cdr (assoc :proxy params)))
         (noproxy (assoc :noproxy params))
         (follow-redirect (and (assoc :follow-redirect params) (not (string= "no" (cdr (assoc :follow-redirect params))))))
         (pretty (assoc :pretty params))
         (prettify (and pretty (not (string= (cdr pretty) "no"))))
         (file (assoc :file params))
         (get-header (cdr (assoc :get-header params)))
         (cookie-jar (cdr (assoc :cookie-jar params)))
         (cookie (cdr (assoc :cookie params)))
         (curl (cdr (assoc :curl params)))
         (select (cdr (assoc :select params)))
         (resolve (cdr (assoc :resolve params)))
         (request-body (ob-api-request-body request))
         (error-output (org-babel-temp-file "curl-error"))
         (args (append ob-api:curl-custom-arguments (list "-i"
                     (when (and proxy (not noproxy)) `("-x" ,proxy))
                     (when noproxy '("--noproxy" "*"))
                     (let ((method (ob-api-request-method request)))
                       (if (string= "HEAD" method) "-I" `("-X" ,method)))
                     (when follow-redirect "-L")
                     (when (and (assoc :username params) (assoc :password params))
                       `("--user" ,(s-format "${:username}:${:password}" 'ob-api-aget params)))
                     (when (assoc :user params) `("--user" ,(cdr (assoc :user params))))
                     (mapcar (lambda (x) `("-H" ,x)) (ob-api-request-headers request))
                     (when (s-present? request-body)
                       (let ((tmp (org-babel-temp-file "http-")))
                         (with-temp-file tmp (insert request-body))
                         `("-d" ,(format "@%s" tmp))))
                     (when cookie-jar `("--cookie-jar" ,cookie-jar))
                     (when cookie `("--cookie" ,cookie))
                     (when resolve (mapcar (lambda (x) `("--resolve" ,x)) (split-string resolve ",")))
                     (when curl (split-string-and-unquote curl))
                     "--max-time"
                     (int-to-string (or (cdr (assoc :max-time params))
                                        ob-api:max-time))
                     "--globoff"
                     (ob-api-construct-url (ob-api-request-url request) params)))))
    (with-current-buffer (get-buffer-create "*curl commands history*")
      (goto-char (point-max))
      (insert "curl "
              (string-join (mapcar 'shell-quote-argument (ob-api-flatten args)) " ")
              "\n"))
    (with-current-buffer (get-buffer-create "*curl output*")
      (erase-buffer)
      (if (= 0 (apply 'call-process "curl" nil `(t ,error-output) nil (ob-api-flatten args)))
          (let ((response (ob-api-parse-response (buffer-string))))
            (when prettify (ob-api-pretty-response response (cdr pretty)))
            (when ob-api:remove-cr (ob-api-remove-carriage-return response))
            (cond (get-header (ob-api-get-response-header response get-header))
                  (select (ob-api-select response select))
                  (prettify (ob-api-pretty-result request response args body params))
                  (file (ob-api-file response (cdr file)))
                  (t (s-join "\n\n" (list (ob-api-response-headers response) (ob-api-response-body response))))))
        (with-output-to-temp-buffer "*curl error*"
          (princ (with-temp-buffer
                   (insert-file-contents-literally error-output)
                   (s-join "\n" (s-lines (buffer-string)))))
          ""))))))

(defun ob-api-export-expand-variables (&optional backend)
  "Scan current buffer for all HTTP source code blocks and expand variables.
Add this function to `org-export-before-processing-hook' to
enable variable expansion before source block is exported."
  (let ((case-fold-search t) elt replacement)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^[ \t]*#\\+begin_src[ \t]+http" nil 'noerror)
        (setq elt (org-element-at-point))
        (when (eq 'src-block (car elt))
          (setq replacement (org-babel-expand-src-block))
          (goto-char (org-element-property :begin elt))
          (delete-region (org-element-property :begin elt) (org-element-property :end elt))
          (insert (org-element-interpret-data (org-element-put-property elt :value replacement))))))))

(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("api" . ob-api)))

(defun org-babel-execute-buffer:api (&optional arg)
  "Execute source code blocks in a buffer.
Call `org-babel-execute-src-block' on every source block in
the current buffer."
  (interactive "P")
  (org-babel-eval-wipe-error-buffer)
  (org-save-outline-visibility t
    (org-babel-map-executables nil
      (when (and (equal 'src-block (org-element-type (org-element-context)))
                 (equal "api" (nth 1 (nth 1 (org-element-context)))))
        (org-babel-execute-src-block arg)))))

(provide 'ob-api)
;;; ob-api.el ends here
