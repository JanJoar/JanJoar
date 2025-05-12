(ql:quickload '(:drakma :xmls))


(defvar template "I am a university student currently studying International Relations
at Stockholm University. I am interested in Lisp-languages, GNU Emacs,
Free software, political economy, and the philosophy of life in an
increasingly technical world.

Here are some recent blog posts of mine:
")


(defun fetch-recent-articles (url &optional (count 3))
  "Fetch the COUNT (default 3) most recent RSS feed entries from URL and return an alist
mapping title to link for each article."
  (let ((xml-string (drakma:http-request url))
        doc channel items)
    (setf doc (xmls:parse xml-string))
    (setf channel (first (xmls:xmlrep-find-child-tags "channel" doc)))
    (setf items (xmls:xmlrep-find-child-tags "item" channel))
    (loop for item in (subseq items 0 (min count (length items)))
          for title-node = (xmls:xmlrep-find-child-tag "title" item)
          for link-node  = (xmls:xmlrep-find-child-tag "link" item)
          for title = (xmls:xmlrep-string-child title-node)
          for link  = (xmls:xmlrep-string-child link-node)
          collect (cons title link))))

(defun format-org-links (articles)
  "Expects an alist of article titles and their links, and outputs
a list of strings formatted as org-mode links."
  (mapcar (lambda (pair)
            (let ((title (car pair))
                  (link (cdr pair)))
              (format nil "- [[~a][~a]]" link title)))
          articles))

(defun fill-in-template (template links)
  "Adds the list of LINKS to the end of TEMPLATE.
LINKS should be a list of org-mode link strings.
Returns the final string."
  (format nil "~a~%~{~a~^~%~}" template links))

(defun write-to-file (text)
  (with-open-file (str "./README.org"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format str text)))

(write-to-file (fill-in-template template (format-org-links (fetch-recent-articles "https://joarvarndt.se/rss.xml" 3))))
