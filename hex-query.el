(require 'url)
(require 'json)
(require 'cl-lib)

(cl-defstruct hex-package
  "Represents a package from the Hex.pm API."
  name
  description
  downloads
  releases
  meta
  github_url
  url
  latest_version
  docs_html_url)

(defun hex-query-package (package-name)
  "Query Hex.pm for a PACKAGE-NAME and return a `hex-package' struct.
Returns nil if the package is not found or an error occurs."
  (let ((url (format "https://hex.pm/api/packages/%s" package-name)))
    (let ((buffer (url-retrieve-synchronously url)))
      (if buffer
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "\n\n") ; Move past headers
            (let* ((json-data (json-read))
                   (meta (cdr (assoc 'meta json-data)))
                   (links (cdr (assoc 'links meta)))
                   (all-downloads (cdr (assoc 'downloads json-data)))
                   (releases-data (cdr (assoc 'releases json-data))))
              (make-hex-package
               :name (cdr (assoc 'name json-data))
               :description (cdr (assoc 'description meta))
               :downloads `(:all ,(cdr (assoc 'all all-downloads))
                                 :week ,(cdr (assoc 'week all-downloads))
                                 :day ,(cdr (assoc 'day all-downloads)))
               :releases (mapcar (lambda (release)
                                   `(:version ,(cdr (assoc 'version release))
                                             :url ,(cdr (assoc 'url release))))
                                 releases-data)
               :meta meta
               :github_url (cdr (assoc 'GitHub links))
               :url (cdr (assoc 'url json-data))
               :latest_version (cdr (assoc 'latest_version json-data))
               :docs_html_url (cdr (assoc 'docs_html_url json-data)))))
        (progn
          (message "Error fetching package %s" package-name)
          nil)))))

(defun hex-search-package (substring)
  "Search for packages on Hex.pm that match SUBSTRING."
  (let ((url (format "https://hex.pm/api/packages?search=%s" substring)))
    (let ((buffer (url-retrieve-synchronously url)))
      (if buffer
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "\n\n") ; Move past headers
            (json-read))
        []))))

(defun hex-query-insert-dependency (package-name)
  "Prompt for a PACKAGE-NAME and insert a mix.exs dependency."
  (interactive "sPackage name: ")
  (let* ((search-results (hex-search-package package-name))
         (completion-table (make-hash-table :test 'equal))
         (selected-package
          (progn
            (mapc (lambda (pkg)
                    (let ((name (cdr (assoc 'name pkg)))
                          (desc (cdr (assoc 'description (cdr (assoc 'meta pkg))))))
                      (puthash name (if (and desc (> (length desc) 40))
                                        (substring desc 0 40)
                                      desc) completion-table)))
                  search-results)
            (completing-read "Select a package: " completion-table nil t))))
    (if selected-package
        (let ((my-package (hex-query-package selected-package)))
          (if my-package
              (let* ((latest-version (hex-package-latest_version my-package))
                     (only-dev-test (y-or-n-p "Only for :dev and :test? "))
                     (version-parts (split-string latest-version "\\."))
                     (truncated-version (if (> (length version-parts) 2)
                                            (mapconcat #'identity (seq-take version-parts 2) ".")
                                          latest-version)))
                (insert (format "{:%s, \"~> %s\"%s}" selected-package truncated-version
                                (if only-dev-test ", only: [:dev, :test], runtime: false" ""))))
            (message "Package not found: %s" selected-package))))))

(defun hex-query-show-package-info (package-name)
  "Query for a PACKAGE-NAME and display its info in a new buffer."
  (interactive "sPackage name: ")
  (let* ((search-results (hex-search-package package-name))
         (completion-table (make-hash-table :test 'equal))
         (selected-package
          (progn
            (mapc (lambda (pkg)
                    (let ((name (cdr (assoc 'name pkg)))
                          (desc (cdr (assoc 'description (cdr (assoc 'meta pkg))))))
                      (puthash name (if (and desc (> (length desc) 40))
                                        (substring desc 0 40)
                                      desc) completion-table)))
                  search-results)
            (completing-read "Select a package: " completion-table nil t))))
    (if selected-package
        (let ((my-package (hex-query-package selected-package)))
          (if my-package
              (with-output-to-temp-buffer (format "*Hex Package: %s*" selected-package)
                (princ (format "Package: %s\n" (hex-package-name my-package)))
                (princ (format "Description: %s\n" (hex-package-description my-package)))
                (princ (format "Latest version: %s\n" (hex-package-latest_version my-package)))
                (princ (format "URL: %s\n" (hex-package-url my-package)))
                (princ (format "GitHub URL: %s\n" (hex-package-github_url my-package)))
                (princ (format "Docs HTML URL: %s\n" (hex-package-docs_html_url my-package))))
            (message "Package not found: %s" selected-package))))))

(defun hex-visit-package-url (url-fn)
  "Helper function to visit a URL for the package at point."
  (let* ((package-name (let ((sexp (thing-at-point 'sexp)))
                         (if (stringp sexp)
                           (substring-no-properties sexp 1)
                         (substring (prin1-to-string sexp) 1)))))
    (if package-name
        (let ((my-package (hex-query-package package-name)))
          (if my-package
              (let ((url (funcall url-fn my-package)))
                (if url
                    (browse-url url)
                  (message "URL not found for %s" package-name)))
            (message "Package not found: %s" package-name)))
      (message "No package at point"))))

(defun hex-visit-package-docs ()
  "Visit the Hex.pm documentation for the package at point."
  (interactive)
  (hex-visit-package-url 'hex-package-docs_html_url))

(defun hex-visit-package-github ()
  "Visit the GitHub page for the package at point."
  (interactive)
  (hex-visit-package-url 'hex-package-github_url))

(defun hex-visit-package-hex ()
  "Visit the Hex.pm page for the package at point."
  (interactive)
  (hex-visit-package-url 'hex-package-url))


;;; Example Usage:
    ;; (let ((my-package (hex-query-package "jason")))
    ;;   (when my-package
    ;;     (message "Package Name: %s" (hex-package-name my-package))
    ;;     (message "Description: %s" (hex-package-description my-package))
    ;;     (message "All-time downloads: %s" (plist-get (hex-package-downloads my-package) :all))
    ;;     (message "Latest version: %s" (hex-package-latest_version my-package))
    ;;     (message "GitHub URL: %s" (hex-package-github_url my-package))
    ;;     (message "URL: %s" (hex-package-url my-package))
    ;;     (message "Docs HTML URL: %s" (hex-package-docs_html_url my-package))))

;;; To use the new function:
;; M-x hex-query-insert-dependency
;; M-x hex-query-show-package-info
;; M-x hex-visit-package-docs
;; M-x hex-visit-package-github
;; M-x hex-visit-package-hex
