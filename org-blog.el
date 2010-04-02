
(defun org-blog-insert-rss-header ()
  (insert
   "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<rdf:RDF 
  xmlns=\"http://purl.org/rss/1.0/\"
  xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" 
  xml:lang=\"ja\">")
  )

(defun org-blog-insert-rss-channel (items)
  (insert (concat
           (format "
<channel rdf:about=\"%s/%s\">
<title>%s</title>
<link>%s</link>
<description>%s</description>
  <items>
   <rdf:Seq>" org-blog-url org-blog-rss-file
   org-blog-title org-blog-url org-blog-rss-description)
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

(defun org-blog-insert-rss-item (item)
  (insert (format "<item rdf:about=\"%s\">
  <title>%s</title>
  <link>%s</link>
  <description>%s</description>
 </item>"
                  (cdr (assoc :about item))
                  (cdr (assoc :title item))
                  (cdr (assoc :link item))
                  (cdr (assoc :title item)))))

(defun org-blog-insert-rss-footer ()
  (insert "</rdf:RDF>\n"))

(defun org-blog-rss-items ()
  "generate rss items alist from articles..."
  ;; about, title, link
  (let ((all-files (remove-if #'null (subseq (org-blog-article-files)
                                             0
                                             org-blog-max-recent-article-num))))
    (let ((all-titles (mapcar #'(lambda (f)
                                  (org-blog-with-temp-buffer
                                   (find-file-noselect f t)
                                   (extract-title-string)))
                              all-files)))
      (mapcar* #'(lambda (fname title)
                   (list (cons :about (org-blog-file-name->url fname))
                         (cons :link fname)
                         (cons :title title)))
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

(defun extract-title-string ()
  (goto-char (point-min))
  ;; search #+TITLE:
  (search-forward "#+TITLE:" nil t)
  (goto-char (match-end 0))
  (let ((start-pos (point)))
    (forward-line)
    (backward-char)
    (let ((end-position (point)))
      (buffer-substring start-pos end-position))))

(defun org-blog-article-file-to-append-string (fname)
  (interactive)
  (org-blog-with-temp-buffer
   (find-file-noselect fname t)
   (org-mode)
   (goto-char (point-min))
   (let ((title (extract-title-string)))
     (re-search-forward (concat "^" outline-regexp) nil t)
     (goto-char (match-beginning 0))
     (let ((from (point)))
       (set-mark (point))
       (goto-char (point-max))
       (org-do-demote)                   ;indent to right
       (concat
        (org-blog-title-to-string title fname)
        (buffer-substring from (point-max)))))))

(defun org-blog-abs-path->rel-path (abs-path)
  (file-relative-name abs-path (expand-file-name org-blog-root-dir)))

(defun org-blog-title-to-string (title fname)
  (format "\n* [[%s][%s]]\n" (org-blog-resolve-org-file-path fname) title))

(defun org-blog-article-directory ()
  (interactive)
  (expand-file-name  org-blog-root-dir))

(defun org-blog-file-name->url (fname)
  (replace-regexp-in-string
   (expand-file-name org-blog-root-dir)
   org-blog-url
   fname t))

(defun org-blog-article-files ()
  (interactive)
  (let ((all-files (directory-files
                    (org-blog-article-directory)
                    t (concat "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-.*\."
                              org-blog-file-suffix "$"))))
    (sort (copy-list all-files) #'file-newer-than-file-p)))

(defun org-blog-misc-org-files ()
  "returns org files which are not articles nor menu"
  (let ((all-files (directory-files
                    (org-blog-article-directory)
                    t (concat org-blog-file-suffix "$")))
        (articles (org-blog-article-files))
        (menu-file (format "%s/%s" org-blog-root-dir org-blog-menu-file)))
    (remove-if
     #'(lambda (x)
         (member x (cons menu-file articles)))
     all-files)))

(defun org-blog-top-page-article-files ()
  (interactive)
  (let ((all-files (org-blog-article-files)))
    (message "%s articles" (length all-files))
    ;; sort file name's by the time-stamp
    (let ((sorted-files (sort (copy-list all-files) #'file-newer-than-file-p)))
      (remove-if #'null (subseq sorted-files 0 org-blog-max-article-num)))))

(defun org-blog-top-page-recent-articles ()
  (interactive)
  (let ((all-files (org-blog-article-files)))
    (let ((sorted-files (sort (copy-list all-files) #'file-newer-than-file-p)))
      (remove-if #'null (subseq sorted-files 0 org-blog-max-recent-article-num)))))

(defun org-blog-insert-title ()
  (insert (concat "# -*- coding: utf-8 -*-
#+STYLE: <link rel=\"alternate\" type=\"application/rss+xml\" title=\"RSS\" href=\""
                  org-blog-url "/" org-blog-rss-file
                  "\" /> 
#+TITLE: " org-blog-title "\n"
                  "#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=\"worg.css\" />\n"
                  )))

(defun org-blog-insert-recent-articles (recent-articles)
  "recent-artciels => list of file paths"
  (insert (apply #'concat "** recent-articles\n"
                 (mapcar #'(lambda (fname)
                             (let ((title (org-blog-with-temp-buffer
                                           (find-file-noselect fname t)
                                           (extract-title-string))))
                               (concat "*** " "[["
                                       (org-blog-resolve-org-file-path fname)
                                       "][" title "]]" "\n")))
                         recent-articles))))

(defmacro org-blog-with-temp-buffer (buf &rest args)
  (let ((b (gensym)))
  `(let ((,b ,buf))
       (unwind-protect
           (with-temp-buffer
             (insert-buffer-substring ,b)
             ,@args)
         (with-current-buffer ,b
           (message "killing %s" (current-buffer))
           (set-buffer-modified-p nil))
         (kill-buffer ,b)))))

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
                                  (org-blog-with-temp-buffer
                                   (find-file-noselect f t)
                                   (extract-title-string)))
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
      (let ((buf (find-file-noselect f t)))
        (with-current-buffer buf
          (org-export-as-html 3)
          (set-buffer-modified-p nil)
          (kill-buffer buf))))))
        

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

(defun org-blog-write-article (file-title title)
  (interactive "sgive me file-name(in ASCII): \nsgive me title(MultiByte character allowed):")
  (let* ((YYYY-MM-DD (format-time-string "%Y-%m-%d" (current-time)))
         (fname (format "%s/%s-%s.%s" org-blog-root-dir
                        YYYY-MM-DD file-title org-blog-file-suffix)))
    (if (file-exists-p fname)
        (find-file fname)
      (with-current-buffer (find-file fname)
        ;; #+TITLE: YYYY-MM-DD title
        (goto-char (point-min))
        (insert "# -*- coding: utf-8 -*-\n")
        (goto-char (point-max))
        (insert (format "#+TITLE: %s %s\n" YYYY-MM-DD title))
        (goto-char (point-max))))))
