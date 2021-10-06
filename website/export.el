(setq org-html-doctype "html5")
(setq org-export-with-section-numbers nil)

(defconst mm.rs/org-mmrs-options '("article"))
(defconst mm.rs/org-export-options-alist
  '((:mmrs "MMRS" "mmrs" mm.rs/org-mmrs-options split)))

(defconst mm.rs/export-root
  (expand-file-name "../" (or (and load-file-name (file-name-directory load-file-name))
                              default-directory)))


(cl-defun mm.rs/expand-file-name (p &optional (root mm.rs/export-root))
  (expand-file-name p root))

(require 'f)

(defun mm.rs/html-head ()
  (f-read-text (mm.rs/expand-file-name "website/uikit-head.html") 'utf-8))

(defconst mm.rs/org-html-translate-template-alist
  `((mm.rs/org-export-always-translate-p . org-html-template)))

(defun mm.rs/org-html-template (contents info)
  (funcall (mm.rs/org-export-get-translater
            contents info mm.rs/org-html-translate-template-alist)
           contents info))
(defconst mm.rs/org-html-headline-alist
  `((identity . org-html-headline)))

(cl-defun mm.rs/org-html--get-headline-printer
    (el &optional (halist mm.rs/org-html-headline-alist))
  (let* ((cns (car halist))
        (pred (car cns))
        (fn (cdr cns))
        (res (funcall pred el)))
    (message "trying %s on headline? %s" pred res )
    (if res
        fn
      (mm.rs/org-html--get-headline-printer el (cdr halist)))))

(defun mm.rs/org-html-headline (headline contents info)
  (funcall (mm.rs/org-html--get-headline-printer headline)
           headline contents info))

(org-export-define-derived-backend
    'mm.rs/html 'html
  :translate-alist `((template . mm.rs/org-html-template)
                     (section . mm.rs/org-html-section)
                     (headline . mm.rs/org-html-headline))
  :options-alist mm.rs/org-export-options-alist)

(defun mm.rs/publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.

PLIST is the property list for the given project.

PUB-DIR is the publishing directory.

Return output file name."
  (org-publish-org-to 'mm.rs/html filename
		      (concat (when (> (length org-html-extension) 0) ".")
			      (or (plist-get plist :html-extension)
				  org-html-extension
				  "html"))
		      plist pub-dir))
