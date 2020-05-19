;;; ob-http-mode.el --- syntax highlight for ob-http

;; Copyright (C) 2020 Jongseok Choi

(require 's)

(setq ob-api-mode-keywords
      (let* ((ob-api-methods
              '(GET POST PUT PATCH DELETE OPTIONS HEAD TRACE CONNECT))
             (ob-api-headers
              '(Accept Accept-Charset Accept-Encoding Accept-Language
                       Accept-Datetime Authorization Cache-Control
                       Connection Cookie Content-Length Content-MD5
                       Content-Type Date Expect From Host If-Match
                       If-Modified-Since If-None-Match If-Range
                       If-Unmodified-Since Max-Forwards Origin Pragma
                       Proxy-Authorization Range Referer TE User-Agent
                       Upgrade Via Warning))
             (ob-api-methods-regexp
              (rx-to-string
               `(seq
                 bol
                 (? (1+ space))
                 (group-n 1 (or ,@(mapcar 'symbol-name ob-api-methods)))
                 space
                 (group-n 2 (1+ any))
                 eol)))
             (ob-api-headers-regexp
              (rx-to-string
               `(seq
                 bol
                 (? (1+ space))
                 (group-n 1 (or ,@(mapcar 'symbol-name ob-api-headers)))
                 ": "
                 (group-n 2 (1+ any))
                 eol)))
             (ob-api-custom-headers-regexp
              "\\(^X-[^ :]+\\): \\(.*\\)$")
             (ob-api-variable-regexp
              "\\([^ ?&=\n]+\\)=\\([^&\n]*\\)")
             (ob-api-misc-regexp
              "\\(&\\|=\\|?\\|{\\|}\\|\\[\\|\\]\\|\\,\\|:\\)"))
        `((,ob-api-headers-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,ob-api-custom-headers-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,ob-api-variable-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,ob-api-methods-regexp  (1 font-lock-constant-face) (2 font-lock-function-name-face))
          (,ob-api-misc-regexp (1 font-lock-comment-face)))))

(define-derived-mode ob-api-mode fundamental-mode "ob api"
  (set (make-local-variable 'font-lock-defaults) '(ob-api-mode-keywords)))

(provide 'ob-api-mode)
;;; ob-api-mode.el ends here
