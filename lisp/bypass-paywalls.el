;;; bypass-paywalls.el --- Get past paywalls in eww  -*- lexical-binding: t; -*-

;; A minimal port of "Bypass Paywalls Clean" to eww/url.el:
;; - per-site request headers (referer, user agent), applied to every
;;   url.el request;
;; - per-site filters that replace a truncated page with the full text
;;   from another source (AMP page, app API, embedded JSON), run just
;;   before eww renders the response.
;; The extension's JS blocking is unnecessary because eww runs no JS.

;;; Code:

(require 'url-http)
(require 'dom)

;; Buffer-local in url-http.el, but only declared there; declare them
;; special here so `let' below binds them dynamically.
(defvar url-http-referer)
(defvar url-http-extra-headers)
(defvar url-http-target-url)

(defconst bypass-paywalls-googlebot
  "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")

(defconst bypass-paywalls-bingbot
  "Mozilla/5.0 (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)")

(defvar bypass-paywalls-sites
  '(("ft.com" :referer "https://www.google.com/")
    ("valor.globo.com" :filter bypass-paywalls-valor)
    ("lemonde.fr" :filter bypass-paywalls-lemonde)
    ("bloomberg.com" :filter bypass-paywalls-bloomberg))
  "Alist of (DOMAIN . PLIST) rules, matching DOMAIN and its subdomains.
PLIST keys:
  :referer     Referer header to send.
  :user-agent  User-Agent to send (e.g. `bypass-paywalls-googlebot').
  :headers     Extra alist of (NAME . VALUE) headers.
  :filter      Function called with the page URL in the response buffer,
               point at the start of the body.  It returns replacement
               HTML for the body, or nil to keep it.")

(defun bypass-paywalls--rule (host)
  "Return the rule plist for HOST, or nil."
  (cdr (seq-find (lambda (r)
                   (or (string= host (car r))
                       (string-suffix-p (concat "." (car r)) host)))
                 bypass-paywalls-sites)))

;;;; Request headers

(defun bypass-paywalls--create-request (orig)
  "Around advice for `url-http-create-request' (ORIG) applying site rules."
  (let ((rule (bypass-paywalls--rule (url-host url-http-target-url))))
    (if (not rule)
        (funcall orig)
      (let ((url-http-referer (or (plist-get rule :referer) url-http-referer))
            (url-user-agent (or (plist-get rule :user-agent) url-user-agent))
            (url-http-extra-headers (append (plist-get rule :headers)
                                            url-http-extra-headers)))
        (funcall orig)))))

;;;; Response filters

(defun bypass-paywalls--render (status url &rest _)
  "Before advice for `eww-render': run the site's :filter on the response."
  (let* ((url (or (plist-get status :redirect) url))
         (filter (plist-get (bypass-paywalls--rule
                             (url-host (url-generic-parse-url url)))
                            :filter)))
    (when filter
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "\r?\n\r?\n" nil t)
          (let* ((body (point))
                 (html (ignore-errors (funcall filter url))))
            (when html
              (delete-region body (point-max))
              (goto-char body)
              (insert (encode-coding-string html 'utf-8)))))))))

(defun bypass-paywalls--fetch (url)
  "Return the body of URL as a decoded string, or nil."
  (when-let* ((buf (url-retrieve-synchronously url t nil 20)))
    (with-current-buffer buf
      (prog1 (when (and (eq (url-http-parse-response) 200)
                        (re-search-forward "\r?\n\r?\n" nil t))
               (decode-coding-string (buffer-substring (point) (point-max))
                                     'utf-8))
        (kill-buffer)))))

(defun bypass-paywalls--body ()
  "Return the response body after point as a decoded string."
  (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8))

(defun bypass-paywalls-valor (url)
  "Valor Econômico: the AMP version carries the full text."
  (when (string-match-p "class=\"paywall" (bypass-paywalls--body))
    (bypass-paywalls--fetch
     (replace-regexp-in-string "\\`https://valor\\.globo\\.com/"
                               "https://valor.globo.com/google/amp/" url))))

(defun bypass-paywalls-lemonde (url)
  "Le Monde: the Android app API returns the full article HTML."
  (when (and (string-match-p "lmd-paywall" (bypass-paywalls--body))
             (string-match "article/.*_\\([0-9]+\\)_" url))
    (let* ((api (concat "https://apps.lemonde.fr/aec/v1/"
                        (if (string-match-p "lemonde\\.fr/en/" url) "en/" "")
                        "premium-android-phone/article/"
                        (match-string 1 url)))
           (vars (alist-get 'template_vars
                            (json-parse-string (bypass-paywalls--fetch api)
                                               :object-type 'alist))))
      (format "<html><head><title>%s</title></head><body>%s</body></html>"
              (alist-get 'seo_title vars) (alist-get 'content vars)))))

(defun bypass-paywalls--bloomberg-html (node)
  "Convert a Bloomberg story body NODE (parsed JSON) to HTML."
  (let* ((data (alist-get 'data node))
         (inner (mapconcat #'bypass-paywalls--bloomberg-html
                           (alist-get 'content node) "")))
    (pcase (alist-get 'type node)
      ("text" (url-insert-entities-in-string (alist-get 'value node)))
      ("paragraph" (format "<p>%s</p>" inner))
      ("heading" (format "<h3>%s</h3>" inner))
      ("blockquote" (format "<blockquote>%s</blockquote>" inner))
      ("list" (format "<ul>%s</ul>" inner))
      ("listItem" (format "<li>%s</li>" inner))
      ("link" (format "<a href=\"%s\">%s</a>" (alist-get 'href data) inner))
      (_ inner))))

(defun bypass-paywalls-bloomberg (_url)
  "Bloomberg: the full story is embedded as JSON in __NEXT_DATA__."
  (let* ((body (bypass-paywalls--body))
         (dom (with-temp-buffer
                (insert body)
                (libxml-parse-html-region (point-min) (point-max))))
         (json (dom-inner-text (car (dom-by-id dom "__NEXT_DATA__"))))
         (story (let-alist (json-parse-string json :object-type 'alist
                                              :array-type 'list)
                  .props.pageProps.story)))
    (when story
      (format "<html><head><title>%s</title></head><body><h1>%s</h1>%s</body></html>"
              (alist-get 'headline story) (alist-get 'headline story)
              (bypass-paywalls--bloomberg-html (alist-get 'body story))))))

;;;###autoload
(define-minor-mode bypass-paywalls-mode
  "Get past paywalls in eww using the rules in `bypass-paywalls-sites'."
  :global t :group 'eww
  (if bypass-paywalls-mode
      (progn
        (advice-add 'url-http-create-request :around #'bypass-paywalls--create-request)
        (advice-add 'eww-render :before #'bypass-paywalls--render))
    (advice-remove 'url-http-create-request #'bypass-paywalls--create-request)
    (advice-remove 'eww-render #'bypass-paywalls--render)))

(provide 'bypass-paywalls)
;;; bypass-paywalls.el ends here
