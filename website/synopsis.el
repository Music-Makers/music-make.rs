(defun mm.rs/org-element--synopsis-abstract (data info)
  "The paragraphs of the first section"
  (let ((info (org-export-get-environment)))
           (remove nil
            (org-element-map
               data 'section
              (lambda (sec)
                (org-element-map
                    sec 'paragraph
                  (lambda (p)
                    (unless (org-html-standalone-image-p p info) p))))
              nil t))))

(defun mm.rs/org-element--synopsis-plist
    (contents info &optional root-dir)
  (let* ((file (plist-get info :input-file))
         (ts (org-export-get-date info))
         (root-dir (or root-dir
                       (file-name-directory file)))

         (title (org-html-plain-text
                 (org-element-interpret-data (plist-get info :title)) info))
         (keys (plist-get info :keywords))
         (date (if ts (org-timestamp-to-time (car ts))
                 (file-attribute-modification-time
	          (file-attributes file))))
         (author (plist-get info :author))
         (url (concat (file-relative-name
                       (file-name-sans-extension filename)
                       root-dir
                       ) ".html") )
         (data (org-element-parse-buffer))
         (abstract (mm.rs/org-element--synopsis-abstract data info))
         (imgp (org-element-map data 'paragraph
                 (lambda (e) (and (org-html-standalone-image-p e info) e)) info t))
         (img (car (org-element-contents imgp)))
         (path (and img (org-element-property :path img)))
         (img-path (and path (file-relative-name path root-dir)))
         (img-attr (org-export-read-attribute :attr_html imgp)))
    (message "URL for synopsis: %s" url)
    (list :input-file file :info keys
          :title title :url url :author author :date date
          :img (and path (append `(:path , img-path) img-attr))
          :abstract abstract :keywords (and keys (split-string keys)))))

(defvar mm.rs/%org-file-syn-plist%)

(defun mm.rs/org-export-synopsis-template (contents info &optional root-dir)
  (setq mm.rs/%org-file-syn-plist%
        (mm.rs/org-element--synopsis-plist
         contents info root-dir))
  (with-output-to-string (princ mm.rs/%org-file-syn-plist%)))

(defun mm.rs/org-file--synopsis-plist (filename &optional root-dir)
  (let* ((mm.rs/%org-file-syn-plist% nil)
       ;  (org-inhibit-startup t)
         (back
          (org-export-create-backend
           :parent 'html
           :transcoders
           `((template . mm.rs/org-export-synopsis-template)))))
 ;; Org provides many ways to execute code blocks. C-c C-c or C-c C-v e with
 ;; the point on a code block calls the `org-babel-execute-src-block`
 ;; function, which executes the code in the block, collects the results, and
 ;; inserts them in the buffer.
 ;;  -- https://orgmode.org/manual/Evaluating-Code-Blocks.html

 ;; You see, (symbol-function SYMBOL) is a valid place expression. Which
 ;; means you can bind it dynamically using cl-letf.
 ;; -- https://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html

    (cl-letf (((symbol-function 'org-babel-execute-src-block)
               (lambda (&optional ARG INFO PARAMS)
                 ;(warn "Not executing for synopsis")
                 "")))
      (mm.rs/with-file-buffer
       filename
       (org-export-as back nil nil nil '(:with-toc nil :eval "no")))
      mm.rs/%org-file-syn-plist%)))



(defun mm.rs/org-file-synopsis-plist (filename)
  "Return a plist for a synopsis of this FILENAME
  
  FILENAME is an orgmode file.
  
  The return value is a plist with the following keys.
  
   - :input-file :: The full name and path the FILENAME points towards
   - :title :: A plain text string of the #+TITLE
   - :url :: a relative (to the current buffer) path to the exported file
   - :author :: A string for the author of this file
   - :date :: An emacs time for #+DATE:
              or if non-existant thelast modification time
   - :img  :: A plist of (:path <path of image> <attr_htmls> ...)
   - :abstract :: a list of the opening paragraphs
   - :keywords :: a list of keywords for meta things
  "
  (setq filename (file-truename filename))
  (let* ((pub? (mm.rs/org-publish-needed-p filename))
         (exist-plist (and (not pub?)
                           (org-publish-cache-get-file-property
                            filename :mm.rs-synopsis-plist))))
    (or exist-plist
        (org-publish-cache-set-file-property
         filename :mm.rs-synopsis-plist
         (mm.rs/org-file--synopsis-plist filename)))))

(defun mm.rs/org-element-synopsis-slider-p (el)
 (and (eq (org-element-type el) 'headline)
      (let ((tgs (org-element-property :tags el)))
        (and (member "mmrs" tgs) (member "slider" tgs) (member "synopsis" tgs)))
      el))






(defvar mm.rs/slider-open-tag-format-string "<!-- SLIDER -->
<div class=\"uk-container\">
  <h4 class=\"uk-heading-line uk-text-bold\"><span>%s</span></h4>
  <div data-uk-slider=\"velocity: 5\">
	<div class=\"uk-position-relative\">
	  <div class=\"uk-slider-container\">
		<ul class=\"uk-slider-items uk-child-width-1-2@m uk-grid uk-grid-medium news-slide\">
")

(defun <mm.rs:slider> (&optional headline)
  (setq headline (or headline "Featured"))
  (format mm.rs/slider-open-tag-format-string headline))

(defconst mm.rs/slider-item-keyword-format-string
  "<span class=\"uk-label uk-label-warning\" style=\"font-size: 0.75rem; margin-right:0.25em \">%s</span>"
  "This takes one %s, the name of the keyword")

(defun <mm.rs:slider-item-keywords/> (keys)
  (unless (listp keys) (setq keys (list keys)))
  (with-output-to-string
    (dolist (k keys)
      (princ (format mm.rs/slider-item-keyword-format-string k)))))

(defconst mm.rs/slider-item-format-time-string
  "%a %b %d, %Y")

(defconst mm.rs/slider-item-format-string
"		  <li>
			<div class=\"uk-card uk-card-default uk-card-body uk-card-small uk-flex uk-flex-middle uk-card-default uk-border-rounded\">
			  <div class=\"uk-grid uk-grid-medium uk-flex uk-flex-middle\" data-uk-grid>
				<div class=\"uk-width-1-3@s uk-width-2-5@m uk-height-1-1\">
                  %s
				</div>
				<div class=\"uk-width-2-3@s uk-width-3-5@m\">
                   %s
				  <h3 class=\"uk-card-title uk-margin-small-top uk-margin-remove-bottom\">
					<a class=\"uk-link-reset\" href=\"%s\">%s</a>
				  </h3>
				  <span class=\"uk-article-meta\">Published %s</span>
				  <p class=\"uk-margin-small\">%s</p>
				</div>
			  </div>
			</div>
		  </li>
")

(cl-defun <mm.rs:slider-item/>
    (&key title url author
          (date '(24907 38903 178018 858000))
          img abstract keywords
          &allow-other-keys)

  " <slider-item/> (&key title author date image abstract keywords)"
  (when (stringp date) (setq date (parse-time-string date)))
  (format mm.rs/slider-item-format-string
          img
          (<mm.rs:slider-item-keywords/> keywords) url title
          (format-time-string mm.rs/slider-item-format-time-string date)
          abstract))

(defvar mm.rs/slider-close-string
"		</ul>
	  </div>
	  <div class=\"uk-hidden@l uk-light\">
		<a class=\"uk-position-center-left uk-position-small\" href=\"#\" data-uk-slidenav-previous data-uk-slider-item=\"previous\"></a>
		<a class=\"uk-position-center-right uk-position-small\" href=\"#\" data-uk-slidenav-next data-uk-slider-item=\"next\"></a>
	  </div>
	  <div class=\"uk-visible@l\">
		<a class=\"uk-position-center-left-out uk-position-small\" href=\"#\" data-uk-slidenav-previous data-uk-slider-item=\"previous\"></a>
		<a class=\"uk-position-center-right-out uk-position-small\" href=\"#\" data-uk-slidenav-next data-uk-slider-item=\"next\"></a>
	  </div>
	</div>
	<ul class=\"uk-slider-nav uk-dotnav uk-flex-center uk-margin\"><li></li></ul>
  </div>
</div>
<!-- /SLIDER -->
")

(defun </mm.rs:slider> () (format mm.rs/slider-close-string))

(defun <mm.rs:slider/> (title &rest items)
  (with-output-to-string
    (princ (<mm.rs:slider> title))
    (princ "\n")
    (dolist (i items)
      (princ (if (listp i)
                 (apply #'<mm.rs:slider-item/> i)
               i)))
    (princ (</mm.rs:slider>))))


(defun mm.rs/org-html-slider (headline contents info)
  (let ((paths (org-element-map headline 'link
                 (lambda (l) (org-element-property :path l)))))
    (message "Trying slider for paths: %s" paths)
    (apply #'<mm.rs:slider/>
           (org-export-data (org-element-property :title headline) info)
           (mapcar (lambda (path) (mm.rs/org-html-file-synopsis path info))
                   paths))))
