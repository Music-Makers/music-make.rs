(require 'f)

(defconst mm.rs/org-html-header-format-string
  (f-read-text "header-format-string.html" 'utf-8))

(cl-defun <mm.rs:header/> ()
  (format mm.rs/org-html-header-format-string))

(defun mm.rs/org-html-header (section contents info)
  (<mm.rs:header/>))

(defun mm.rs/org-html-header-section (section contents info)
  (concat (mm.rs/org-html-header section contents info) "\n"
          (org-html-section section contents info)))

(defun mm.rs/org-export-header-section-p (el info)
  (and (eq (org-element-type el) 'section)
       (not (org-export-get-parent-headline el))))

(add-to-list
 'mm.rs/org-html-translate-section-alist
 '(mm.rs/org-export-header-section-p . mm.rs/org-html-header-section))
