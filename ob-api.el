;;; ob-api.el --- http request in org-mode babel

;; Copyright (C) 2020 Jongseok Choi
;;
;; This code has been derived from ob-http.el

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
    (baseurl . :any)
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
    (:exports . "both"))
  "Default arguments for evaluating a restclient block.")

(defvar org-babel-deps-handle:api nil "Indicate deps handle")

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
  (let* ((ret (s-format body 'ob-api-aget
                        (mapcar (lambda (x) (when (eq (car x) :var) (cdr x))) params)))
         (rem (s-count-matches "\\$\\({\\([^}]+\\)}\\|[0-9]+\\)" ret)))
    (if (> rem 0) (org-babel-expand-body:api ret params) ret)))

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
       (org-babel-expand-body:api (format "%s" (or (assoc-default :baseurl params) "http://localhost:8080")) params) 
       path)
    path))

(defun ob-api-file (response filename)
  (let ((body (ob-api-response-body response)))
    (with-temp-file filename
      (insert body))))

(defun ob-api-manipulate-args (args)
  "table parameters will be manipulated."
  (let* ((ret '())
         (dval '())
         (delay 0))
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
            ((eq (car el) :desc)
             (let* ((desc (s-split "\" "  (cdr el))))
               (dolist (el desc)
                 (let* ((d (s-split "=" (replace-regexp-in-string "\"" "" el ))))
                   (push (append '(:desc) (cons (intern (nth 0 d)) (nth 1 d))) ret)))))
            ((eq (car el) :optional)
             (let* ((opts (s-split " " (cdr el))))
               (dolist (el opts)
                 (push (append '(:optional) (cons (intern el) "")) ret))))
            ((eq (car el) :deps)
             (let* ((deps (s-split " " (cdr el)))
                    (old org-babel-deps-handle:api))
               (setq org-babel-deps-handle:api t)
               (condition-case nil
                   (dolist (el deps) (push (org-babel-ref-parse el) dval))
                 (error nil))
               (setq org-babel-deps-handle:api old)))
            ((eq (car el) :eval)
             (let* ((evals (s-split " " (cdr el))))
               (dolist (el evals)
                 (let* ((es (s-split "=" el))
                        (vn (intern (nth 0 es)))
                        (de (s-split "#" (nth 1 es)))
                        (resp (cdr (assoc (intern (nth 0 de)) dval)))
                        (v (s-replace "\n" ""  (ob-api-select resp (nth 1 de)))))
                   (push (append '(:var) (cons vn v)) ret)))))
            ((eq (car el) :delay)
             (set 'delay (cdr el)))
            (t (push el ret))))
    (sleep-for delay)
    ret))

(defun ob-api-headers-to-table (headers spaces)
  "returns table string"
  (string-join (mapcar (lambda (x)  (format "%s| %s |\n" spaces (s-join "|" (s-split-up-to ":" x 1)))) headers)))

(defun ob-api-body-descriptor:json (body params)
  "returns description string of request body and description"
  (let* ((ret '())
         (vars (mapcar (lambda (x) (when (eq (car x) :var) (cdr x))) params))
         (descs (mapcar (lambda (x) (when (eq (car x) :desc) (cdr x))) params))
         (opts (mapcar (lambda (x) (when (eq (car x) :optional) (cdr x))) params))
         )
    (replace-regexp-in-string
     "\\$\\({\\([^}]+\\)}\\|[0-9]+\\)"
     (lambda (md)
       (let* ((name (substring md 2 -1))
              (desc (cdr (assoc (intern name) descs)))
              (val (cdr (assoc (intern name) vars)))
              (required (if (assoc (intern name) opts) "No" "Yes"))
              (type (cond
                     ((numberp val) "number")
                     ((booleanp val) "boolean")
                     ((stringp val) "string")
                     (t "object"))))
         (add-to-list 'ret (format "  | %s | %s | %s | %s |" name desc required type) t)
         md))
     body)
    (string-join ret "\n")))

(defun composite-curl-command (request params)
  (let* ((headers (ob-api-request-headers request))
         (body (ob-api-request-body request))
         (method (ob-api-request-method request))
         (url (ob-api-construct-url (ob-api-request-url request) params))
         (header-command (mapcar (lambda (x) (format "-H '%s'" x)) headers))
         )
    (string-join (list
                  (format "curl -i -X %s" method)
                  (when header-command
                    (string-join header-command " "))
                  (format "--max-time 30 --globoff '%s'" url)
                  (when body
                    (format "-d '%s'" (s-replace "\n" " " body)))
                  "\n") " ")))

(defun json-extractor-type (jsonobj &optional basename)
  (let* ((ret (list)))
    (dolist (x jsonobj)
      (let* ((val (cdr x))
             (key (if basename (format "%s.%s" basename (car x)) (car x))))
        (setq ret (append ret 
         (cond
          ((stringp val) (list (cons key "string")))
          ((numberp val) (list (cons key "number")))
          ((booleanp val)  (list (cons key "boolean")))
          ((listp val) (append (list (cons key "object")) (json-extractor-type val key)))
          ((arrayp val) (list (cons key "array")))
          (t (list (cons key "unknown"))))))))
    ret))

(defun ob-api-pretty-result (request response args body params)
  "returns result string"
  (let* ((req (ob-api-parse-request body))
         (headers-body (ob-api-split-header-body body))
         (headers (s-split-up-to "\\(\r\n\\|[\n\r]\\)" (car headers-body) 1))
         (url (cadr (split-string (car headers) " ")))
         (sbody (cadr headers-body))
         (hdr (cadr headers))
         (resp-body (ob-api-response-body response))
         (rbody (when (and resp-body (not (equal resp-body ""))) resp-body))
         (udescs (ob-api-body-descriptor:json url params))
         (hdescs (when hdr (ob-api-body-descriptor:json hdr params)))
         (bdescs (when sbody (ob-api-body-descriptor:json sbody params))))
    (concat
     (when (and hdescs (not (equal "" hdescs)))
       (concat
        "+ *Request Headers*\n"
        "  | Name | Description | Required | Type |\n"
        "  |------+-------------+----------+------|\n"
        hdescs " \n"
        " \n"))
     (when (and udescs (not (equal "" udescs)))
       (concat
        "+ *Path Parameters*\n"
        "  | Name | Description | Required | Type |\n"
        "  |------+-------------+----------+------|\n"
        udescs " \n"
        "  \n"))
     (when (and bdescs (not (equal "" bdescs)))
       (concat
        "+ *Body Parameters*\n"
        "  | Name | Description | Required | Type |\n"
        "  |------+-------------+----------+------|\n"
        bdescs "  \n"
        "  \n"))
     "+ *Example*\n"
     "  + Command\n"
     "  #+BEGIN_SRC sh\n"
     (composite-curl-command request params)
     "  #+END_SRC\n"
     " \n"
     "  + Request\n"
     "  #+BEGIN_SRC js\n"
     (format "// %s %s\n" (ob-api-request-method request) (ob-api-request-url request))
     (when (ob-api-request-headers request)
       (format "// %s\n" (string-join (ob-api-request-headers request) "\n// ")))
     (when (ob-api-request-body request)
       (format "\n%s" (ob-api-pretty-json (ob-api-request-body request))))
     "  #+END_SRC\n"
     " \n"
     "  + Response\n"
     "\n"
     (let* ((ht (s-split-up-to " " (car (s-split-up-to "\n" (s-replace "" "" (ob-api-response-headers response)) 1)) 2)))
       (format  "    /*%s*/ - %s\n" (nth 1 ht) (nth 2 ht)))
     (when rbody
       (concat 
       "    | Name | Type |\n"
       "    |------+------|\n"
       (let* ((j (json-read-from-string rbody)))
         (format "%s\n"  (string-join
                        (mapcar
                         (lambda (x)
                           (format "    | %s | %s |" (car x) (cdr x))
                           ) (json-extractor-type j)) "\n")))))
     "\n"
     "  #+BEGIN_SRC js\n"
     (when (ob-api-response-headers response)
       (format "// %s \n" (s-replace "" "" (s-replace "\n" "\n// " (ob-api-response-headers response)))))
     " \n"
     (when rbody
       (format "%s"  (ob-api-pretty-response response "yes")))
     "  #+END_SRC\n")))

(defun org-babel-append-header-to-body (body params)
  (let* ((headers (org-babel-ref-resolve (cdr (assoc :headers params))))
         (sbody (s-split-up-to "\n" body 1))
         (ret '()))
    (dolist (el headers ret) (push (format "%s: %s" (nth 0 el) (nth 1 el)) ret))
    (let* ((h (string-join ret "\n"))
           (b (string-join (list (nth 0 sbody) h) "\n")))
      (if (> (length sbody) 1) (string-join (list b (nth 1 sbody)) "\n") b))))

(defun org-babel-execute:api (ibody args)
  (let* ((params (ob-api-manipulate-args args))
         (body (org-babel-append-header-to-body ibody params)))
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
                                                              `("-d" ,(format "%s" request-body)))
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
                    (org-babel-deps-handle:api response)
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
