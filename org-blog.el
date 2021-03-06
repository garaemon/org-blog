;; org-blog.el
;;
;;

(require 'cl)

;; global symbols...
(defvar org-blog-root-dir "~/org/example.com")
(defvar org-blog-output-root-dir "~/org/example.com-output")
(defvar org-blog-menu-file "menu.org")
(defvar org-blog-title "example.com: this is my blog")
(defvar org-blog-url "http://example.com/org")
(defvar org-blog-rss-description "this is rss feed generated by org-blog")
(defvar org-blog-rss-file "rss.rdf")
(defvar org-blog-file-suffix "org")
(defvar org-blog-update-command "upload-script.sh")
(defvar org-blog-max-article-num 5)
(defvar org-blog-max-recent-article-num 15)
(defvar org-blog-styles nil)

(defmacro org-blog-with-temp-buffer (buf &rest args)
  "This macro takes a buffer `buf', copy the texts of `buf'
to a temporary buffer, and evaluate `args' with binding the temporary buffer
to current-buffer. The return value is the last value of `args'."
  (let ((b (gensym)))
    `(let ((,b ,buf))
       (with-temp-buffer
         (insert-buffer-substring ,b)
         ,@args))
    ))

(defmacro* org-blog-kill-buffer-if-not-visited ((buf bufname) &rest args)
  "This macro takes buffer name and an object of buffer.
First, this macro checks if there is the buffer whose name is equal to bufname.
If there is, this macro do nothing, just eval `args' with binding `buf'
to `current-buffer'.
If there isn't, this macro call `kill-buffer' without any warnings after
evaluate `args'.
NB: This macro test by calling `get-buffer'"
  (let ((buffer-exist-p (gensym))
        (target-buffer (gensym)))
    `(let ((,buffer-exist-p (get-buffer ,bufname)))
       (let ((,target-buffer ,buf))
         (unwind-protect
             (with-current-buffer ,target-buffer
               ,@args)
           (unless ,buffer-exist-p
             (with-current-buffer ,target-buffer
               (set-buffer-modified-p nil))
             (kill-buffer ,target-buffer))
           )))
    ))

(defmacro* org-blog-kill-file-buffer-if-not-visited ((file-name) &rest args)
  "This macro is the specialized macro of `org-blog-kill-buffer-if-not-visited'.
This macro takes only `file-name'. This macro is required because, file-buffer
returns the file name without directory paths by `get-buffer' as the name, but
we want to check by absolute path."
  (let ((buffer (gensym))
        (buffer-exist-p (gensym)))
    `(let ((,buffer-exist-p (get-file-buffer (expand-file-name ,file-name))))
       ;; if you already visit the file, ,buffer-exist-p is an instance
       ;; of buffer.
       (let ((,buffer (or ,buffer-exist-p (find-file-noselect ,file-name))))
         (unwind-protect
             (with-current-buffer ,buffer
               ,@args)
           (unless ,buffer-exist-p
             (with-current-buffer ,buffer
               (set-buffer-modified-p nil))
             (kill-buffer ,buffer)))))
    ))

(defun org-blog-extract-title-string ()
  "returns the title of current-buffer.
Titles are following #+TITLE:.
After calling this function, point is located the next line of #+TITLE: foovar."
  (interactive)
  (goto-char (point-min))
  ;; search #+TITLE:
  (re-search-forward "^#\\+TITLE:" nil t)
  (goto-char (match-end 0))
  (let ((start-pos (point)))
    (forward-line)
    (backward-char)
    (let ((end-position (point)))
      (buffer-substring start-pos end-position))))

(defun org-blog-article-file-to-append-string (fname)
  "org-blog generate the file which is called appended article file.
This function takes a file name, that is expected to be org file,
and returns the string which are converted for appended article file."
  (interactive)
  (org-blog-kill-file-buffer-if-not-visited ;if not visited `fname', we kill
   (fname)                     ;the buffer after calling this function
   (org-mode)                  ;turn on org-mode for current-buffer
   (goto-char (point-min))     ;goto the begining of buffer
   (let ((title (org-blog-extract-title-string)))
     ;; go to the beginning of contents of org-mode
     (re-search-forward (concat "^" outline-regexp) nil t)
     (goto-char (match-beginning 0))
     (let ((from (point)))
       (push-mark (point))
       (goto-char (point-max))
       (org-do-demote)                  ;indent to right
       (unwind-protect
           (concat
            (org-blog-title-to-string title fname)
            (buffer-substring from (point-max)))
         (org-do-promote)
         (pop-mark))))))

(defun org-blog-abs-path->rel-path (abs-path)
  "convert `abs-path', that is absolute path of local file system, to
url-path, that is absolute path of remove server file system."
  (file-relative-name abs-path (expand-file-name org-blog-root-dir)))

(defun org-blog-title-to-string (title fname)
  "This function takes a title: `title' and the file name `fname', and
returns the title string which has the link to `fname' in output file system."
  (format "\n* [[%s][%s]]\n" (org-blog-resolve-org-file-path fname) title))

(defun org-blog-article-directory ()
  "This function returns the path to the directory which has the articles of
org-blog."
  (interactive)
  (expand-file-name org-blog-root-dir))

(defun org-blog-file-name->url (fname)
  "This function take a file name `fname' and returns the url of the
remote server. This function expects `fname' is in `org-blog-root-dir'.
This function also replaces file name suffix .org to .html."
  (interactive)
  (replace-regexp-in-string
   (concat "\\." org-blog-file-suffix "$")
   "\.html"
   (replace-regexp-in-string
    (expand-file-name org-blog-root-dir)
    org-blog-url
    fname t) t))

(defun org-blog-get-date-from-article-file (fname)
  "This function takes a file name of article and returns
a associated list of date."
  (interactive)
  (let ((f (file-name-nondirectory fname)))
    (destructuring-bind (YYYY MM DD hh min ss &rest title)
        (split-string f "-")
      (list (cons :year (string-to-number YYYY))
            (cons :month (string-to-number MM))
            (cons :day (string-to-number DD))
            (cons :hour (string-to-number hh))
            (cons :minute (string-to-number min))
            (cons :sec (string-to-number ss))))))

(defun org-blog-article-newer-than-p (x y)
  "This function returns t when x is newer than y using date estimation
by `org-blog-get-date-from-article-file'. This function expects the arguments
to be YYYY-MM-DD-title format. It's an ugly implementation..."
  (interactive)
  (let ((x-date (org-blog-get-date-from-article-file x))
        (y-date (org-blog-get-date-from-article-file y)))
    (cond
     ((> (cdr (assoc :year x-date)) (cdr (assoc :year y-date)))
      t)
     ((< (cdr (assoc :year x-date)) (cdr (assoc :year y-date)))
      nil)
     ((> (cdr (assoc :month x-date)) (cdr (assoc :month y-date)))
      t)
     ((< (cdr (assoc :month x-date)) (cdr (assoc :month y-date)))
      nil)
     ((> (cdr (assoc :day x-date)) (cdr (assoc :day y-date)))
      t)
     ((< (cdr (assoc :day x-date)) (cdr (assoc :day y-date)))
      nil)
     ((< (cdr (assoc :hour x-date)) (cdr (assoc :hour y-date)))
      nil)
     ((> (cdr (assoc :hour x-date)) (cdr (assoc :hour y-date)))
      t)
     ((< (cdr (assoc :minute x-date)) (cdr (assoc :minute y-date)))
      nil)
     ((> (cdr (assoc :minute x-date)) (cdr (assoc :minute y-date)))
      t)
     ((< (cdr (assoc :sec x-date)) (cdr (assoc :sec y-date)))
      nil)
     ((> (cdr (assoc :sec x-date)) (cdr (assoc :sec y-date)))
      t)
     (t
      (file-newer-than-file-p x y)))))

(defun org-blog-article-files ()
  "`org-blog-article-files' returns a list of file names of articles.
The list is sorted by date, NOT time stamp."
  (interactive)
  (let ((all-files (directory-files
                    (org-blog-article-directory)
                    t (concat "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]-.*\."
                              org-blog-file-suffix "$"))))
    (sort (copy-list all-files) #'org-blog-article-newer-than-p)))

(defun org-blog-misc-org-files ()
  "returns org files which are not articles nor menu file."
  (let ((all-files (directory-files
                    (org-blog-article-directory)
                    t (concat "\\." org-blog-file-suffix "$")))
        (articles (org-blog-article-files))
        (menu-file (format "%s/%s" org-blog-root-dir org-blog-menu-file)))
    (remove-if #'(lambda (x)
                   (member x (cons menu-file articles)))
               all-files)))

(defun org-blog-top-page-article-files ()
  (interactive)
  (let ((all-files (org-blog-article-files)))
    ;; sort file name's by the time-stamp
    (remove-if #'null (subseq all-files 0 org-blog-max-article-num))))

(defun org-blog-top-page-recent-articles ()
  (interactive)
  (let ((all-files (org-blog-article-files)))
    (let ((sorted-files
           (sort (copy-list all-files) #'org-blog-article-newer-than-p)))
      (remove-if #'null
                 (subseq sorted-files 0 org-blog-max-recent-article-num)))))

(defun org-blog-insert-title ()
  (insert (concat "# -*- coding: utf-8 -*-
#+STYLE: <link rel=\"alternate\" type=\"application/rss+xml\" title=\"RSS\" href=\""
                  org-blog-url "/" org-blog-rss-file
                  "\" /> 
#+TITLE: " org-blog-title "\n"))
  ;; insert styles
  (dolist (s org-blog-styles)
    (insert (concat "#+STYLE: " s "\n")))
  ;;"#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=\"worg.css\" />\n"
  )

(defun org-blog-insert-recent-articles (recent-articles)
  "recent-artciels => list of file paths"
  (insert (apply #'concat "** recent-articles\n"
                 (mapcar #'(lambda (fname)
                             (let ((title
                                    (org-blog-kill-file-buffer-if-not-visited
                                     (fname)
                                     (org-blog-extract-title-string))))
                               (concat "*** " "[["
                                       (org-blog-resolve-org-file-path fname)
                                       "][" title "]]" "\n")))
                         recent-articles))))

(defun org-blog-insert-link-to-all-articles ()
  (insert "\n*** [[file:all-articles.org][all-articles...]]\n"))

(defun org-blog-resolve-org-file-path (abs-path)
  (format "file:./%s"
          (file-relative-name (replace-regexp-in-string
                               (expand-file-name org-blog-root-dir)
                               (expand-file-name org-blog-output-root-dir)
                               abs-path t)
                              (concat org-blog-current-target-file "/.."))))

(defun* org-blog-insert-menu ()
  (let ((menu-file (format "%s/%s" org-blog-root-dir org-blog-menu-file))
        (recent-articles (org-blog-top-page-recent-articles)))
    ;; first of all, insert menu-file
    (insert-file-contents menu-file)
    (goto-char (point-max))
    ;; generate recent articles
    (org-blog-insert-recent-articles recent-articles)
    (goto-char (point-max))
    (org-blog-insert-link-to-all-articles)
    (goto-char (point-max))
    ))

(defun org-blog-generate-top-page (fname)
  (let ((article-files (org-blog-top-page-article-files)))
    (let ((articles-in-strings (mapcar #'org-blog-article-file-to-append-string
                                       article-files)))
      (with-temp-file fname
        (goto-char (point-min))
        ;; title
        (org-blog-insert-title)
        (goto-char (point-max))
        ;; menu
        (org-blog-insert-menu)
        (dolist (article articles-in-strings)
          (goto-char (point-max))
          (insert article))
        ))))

(defun org-blog-gen-top ()
  (interactive)
  (let ((fname (format "%s/top.org" org-blog-output-root-dir)))
    (let ((org-blog-current-target-file fname))
      (let ((article-files (org-blog-top-page-article-files)))
        (let ((articles-in-strings
               (mapcar #'org-blog-article-file-to-append-string
                       article-files)))
          (with-temp-file fname
            (goto-char (point-min))
            ;; title
            (org-blog-insert-title)
            (goto-char (point-max))
            ;; menu
            (org-blog-insert-menu)
            (dolist (article articles-in-strings)
              (goto-char (point-max))
              (insert article))))))))

(defun org-blog-gen-all-articles ()
  ;; gather path and title of all articles...
  (let ((all-files (org-blog-article-files)))
    (let ((all-titles (mapcar #'(lambda (f)
                                  (org-blog-kill-file-buffer-if-not-visited
                                   (f)
                                   (org-blog-extract-title-string)))
                              all-files)))
      ;; generate org file...
      (let ((fname (format "%s/all-articles.org" org-blog-output-root-dir)))
        (let ((org-blog-current-target-file fname))
          (with-temp-file fname
            (goto-char (point-min))
            (org-blog-insert-title)
            (goto-char (point-max))
            (org-blog-insert-menu)
            (goto-char (point-max))
            (do ((%file all-files (cdr %file))
                 (%title all-titles (cdr %title)))
                ((null %file))
              (let ((file (car %file))
                    (title (car %title)))
                (message "... %s -> %s " title file)
                (insert "* [[" (org-blog-resolve-org-file-path file)
                        "][" title "]]\n")))))))))

(defun org-blog-org-file-input->output (input)
  (replace-regexp-in-string
   (expand-file-name org-blog-root-dir)
   (expand-file-name org-blog-output-root-dir)
   input t))

(defun org-blog-gen-each-articles ()
  (let* ((all-files (org-blog-article-files))
         (target-files (mapcar #'org-blog-org-file-input->output all-files)))
    ;; check need to convert or not
    (mapcar* #'(lambda (from to)
                 (org-blog-gen-each-article-file from to))
             all-files target-files)))

(defun org-blog-gen-each-article-file (fname to)
  (let ((org-blog-current-target-file to))
    (let ((str (org-blog-article-file-to-append-string fname)))
      (with-temp-file to
        (goto-char (point-min))
        ;; title
        (org-blog-insert-title)
        (goto-char (point-max))
        ;; menu
        (org-blog-insert-menu)
        (goto-char (point-max))
        (insert str)))))

(defun org-blog-gen-misc-pages ()
  (let* ((all-files (org-blog-misc-org-files))
         (target-files (mapcar #'org-blog-org-file-input->output all-files)))
    (mapcar* #'(lambda (from to)
                 (when (file-newer-than-file-p from to)
                   (org-blog-gen-each-article-file from to)))
             all-files target-files)))

(defun org-blog-gen-org-files ()
  (interactive)
  (org-blog-gen-top)                    ;done
  (org-blog-gen-all-articles)           ;done
  (org-blog-gen-each-articles)
  (org-blog-gen-misc-pages)
  (org-blog-gen-rss)
  )

(defun org-blog-all-tmp-files ()
   (directory-files (expand-file-name org-blog-output-root-dir)
                    t (concat ".*\\." org-blog-file-suffix "$")))

(defun org-blog-export-to-html ()
  (interactive)
  (let ((files (org-blog-all-tmp-files)))
    (dolist (f files)
      (org-blog-kill-file-buffer-if-not-visited
       (f)
       (org-export-as-html 3)))))

;; for rss
(defun org-blog-insert-rss-header ()
  (insert
   "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<rdf:RDF 
  xmlns=\"http://purl.org/rss/1.0/\"
  xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
  xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
  xml:lang=\"ja\">")
  )

(defun org-blog-insert-rss-channel (items)
  (insert (concat
           (format "
<channel rdf:about=\"%s/%s\">
<title>%s</title>
<link>%s</link>
<description>%s</description>
  <dc:date>%s</dc:date>
  <items>
   <rdf:Seq>" org-blog-url org-blog-rss-file
   org-blog-title org-blog-url org-blog-rss-description
   (org-blog-rss-date-string (car items)))
           (apply #'concat
                  (mapcar #'(lambda (item)
                              (format "<rdf:li rdf:resource=\"%s\" />\n"
                                      (cdr (assoc :about item))))
                          items))
           "</rdf:Seq>
  </items>
 </channel>")))

(defun org-blog-insert-rss-items (items)
  (dolist (item items)
    (org-blog-insert-rss-item item)))

(defun org-blog-rss-date-string (items)
  (format "%s-%02d-%02dT%02d:%02d:%02d+%s"
          (cdr (assoc :year items))
          (cdr (assoc :month items))
          (cdr (assoc :day items))
          (cdr (assoc :hour items))
          (cdr (assoc :minute items))
          (cdr (assoc :sec items))
          (org-blog-zone-offset-string)))

(defun org-blog-insert-rss-item (item)
  (insert (format "<item rdf:about=\"%s\">
  <title>%s</title>
  <link>%s</link>
  <description>%s</description>
  <dc:date>%s</dc:date>
 </item>"
                  (cdr (assoc :about item))
                  (cdr (assoc :title item))
                  (cdr (assoc :link item))
                  (cdr (assoc :title item))
                  (org-blog-rss-date-string item)
                  )))

(defun org-blog-insert-rss-footer ()
  (insert "</rdf:RDF>\n"))

(defun org-blog-rss-items ()
  "generate rss items alist from articles..."
  ;; about, title, link
  (let ((all-files (remove-if #'null (subseq (org-blog-article-files)
                                             0
                                             org-blog-max-recent-article-num))))
    (let ((all-titles (mapcar #'(lambda (f)
                                  (org-blog-kill-file-buffer-if-not-visited
                                   (f)
                                   (org-blog-extract-title-string)))
                              all-files)))
      (mapcar* #'(lambda (fname title)
                   (append
                    (list (cons :about (org-blog-file-name->url fname))
                          (cons :link (org-blog-file-name->url fname))
                          (cons :title title))
                    (org-blog-get-date-from-article-file fname)))
               all-files all-titles))))

(defun org-blog-gen-rss ()
  (let ((rss-file (concat org-blog-output-root-dir "/" org-blog-rss-file)))
    (with-temp-file rss-file
      (let ((rss-items (org-blog-rss-items)))
        (goto-char (point-max))
        (org-blog-insert-rss-header)
        (goto-char (point-max))
        (org-blog-insert-rss-channel rss-items)
        (goto-char (point-max))
        (org-blog-insert-rss-items rss-items)
        (goto-char (point-max))
        (org-blog-insert-rss-footer)
        ))
    ))

(defun org-blog-publish ()
  (interactive)
  (org-blog-gen-org-files)
  (org-blog-export-to-html)
  )

(defun org-blog-publish-and-upload ()
  (interactive)
  (org-blog-publish)
  (shell-command org-blog-update-command)
  )

(defun org-blog-zone-offset-string ()
  (let ((offset (elt (decode-time (current-time)) 8)))
    (let* ((h (/ offset (* 60 60)))
           (m (/ (- offset (* h 60 60)) 60)))
      (format "%02d:%02d" h m))))

(defun org-blog-write-article (file-title title)
  (interactive
   "sgive me file-name(in ASCII): \nsgive me title(MultiByte character allowed):")
  (let* ((YYYY-MM-DD-hh-mm-ss
          (format-time-string "%Y-%m-%d-%H-%M-%S" (current-time)))
         (fname (format "%s/%s-%s.%s" org-blog-root-dir
                        YYYY-MM-DD-hh-mm-ss
                        file-title org-blog-file-suffix)))
    (if (file-exists-p fname)
        (find-file fname)
      (with-current-buffer (find-file fname)
        ;; #+TITLE: YYYY-MM-DD-hh-mm-ss title
        (goto-char (point-min))
        (insert "# -*- coding: utf-8 -*-\n")
        (goto-char (point-max))
        (insert (format "#+TITLE: %s %s\n" YYYY-MM-DD-hh-mm-ss title))
        (goto-char (point-max))))))

(provide 'org-blog)
