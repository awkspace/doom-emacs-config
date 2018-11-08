;;; my-templates/config.el -*- lexical-binding: t; -*-

(defvar my-file-templates-dir
  (expand-file-name "templates/" (file-name-directory load-file-name))
  "The path to a directory of yasnippet folders to use for file templates.")

(after! yasnippet
  (add-to-list 'yas-snippet-dirs 'my-file-templates-dir 'append #'eq)
  (yas-reload-all))

(set-file-template! "\\.md$"
  :when #'(lambda (file)
            (or (cl-search "_drafts/" file)
                (cl-search "_posts" file)))
  :trigger "__jekyll-blog"
  :mode 'markdown-mode)

