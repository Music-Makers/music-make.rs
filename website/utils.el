(defun mm.rs/org-html-standalone-image-p (p &optional info)
  (org-html-standalone-image-p p info))

(defun mm.rs/org-html-image (element)
  (let* ((link (if (eql (org-element-type element) 'link) element
                 (org-element-map element 'link #'identity nil t)))
         (p (if (eql (org-element-type element) 'paragraph) element
              (org-element-property :parent link))))
    (org-html--format-image
     (org-element-property :path link)
     (org-export-read-attribute :attr_html p)
     '(:html-doctype "html"))))
