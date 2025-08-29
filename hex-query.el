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
    (condition-case err
        (let ((buffer (url-retrieve-synchronously url)))
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
               :docs_html_url (cdr (assoc 'docs_html_url json-data))))))
      ((error) (message "Error fetching package %s: %s" package-name err)
       nil))))

(defun hex-query-insert-dependency (package-name)
  "Prompt for a PACKAGE-NAME and insert a mix.exs dependency."
  (interactive "sPackage name: ")
  (let* ((my-package (hex-query-package package-name))
         (latest-version (hex-package-latest_version my-package)))
    (if my-package
        (let* ((only-dev-test (y-or-n-p "Only for :dev and :test? "))
               (version-parts (split-string latest-version "\\."))
               (truncated-version (if (> (length version-parts) 2)
                                      (mapconcat #'identity (seq-take version-parts 2) ".")
                                    latest-version)))
          (insert (format "{:%s, \"~> %s\"%s}" package-name truncated-version
                          (if only-dev-test ", only: [:dev, :test], runtime: false" ""))))
      (message "Package not found: %s" package-name))))


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
