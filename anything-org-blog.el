;; anything-org-blog
;;
;; written by R.Ueda (garaemon)

(require 'anything)

(defvar anything-c-org-blog-articles
  '((name . "org-blog articles")
    (candidates . (lambda ()
                    (let ((article-file-names (org-blog-article-files)))
                      article-file-names)))
    (action .  (("Open" . find-file)))))

(defvar anything-c-org-blog-pages
  '((name . "org-blog pages")
    (candidates . (lambda ()
                    (org-blog-misc-org-files)))
    (action . (("Open" . find-file)))))


(defvar anything-c-org-blog-functions
  '((name . "org-blog functions")
    ;; (function-name description)
    (candidates . ("org-blog-write-article write a new article!"
                   "org-blog-publish-and-upload convert the all articles to html and upload them"
                   "org-blog-publish convert the all articles to html"
                   ))
    (action . (("Eval" . (lambda (x)
                           (call-interactively (intern (car (split-string x)))))))
            )))

(setq anything-c-org-blog-sources
      (list anything-c-org-blog-functions anything-c-org-blog-articles))

(defun anything-org-blog ()
  (interactive)
  (anything anything-c-org-blog-sources)
  )

(provide 'anything-org-blog)
