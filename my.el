(defcustom my-post-directory "~/src/chrislo/chrislowis.co.uk/_posts/"
  "Directory for blog posts"
  :type 'string)

(defun my/create-link-post ()
  "Create a new link post with URL and title"
  (interactive)
  (let* ((url (read-string "URL: "))
         (title (read-string "Title: "))
         (date (format-time-string "%Y-%m-%d"))
         (kebab-title (replace-regexp-in-string " " "-" (downcase title)))
         (filename (concat my-post-directory date "-" kebab-title ".md"))
         (content (format "---
layout: post
title: %s
link: %s
published: true
category: link
---

[{{ page.title }}]({{ page.link }})

" title url)))
    (find-file filename)
    (insert content)
    (goto-char (point-max))))

(defun my/toggle-journal ()
  "Toggle between journal and previous buffer."
  (interactive)
  (if (and (buffer-file-name)
           (string-match-p denote-journal-extras-keyword (buffer-file-name)))
      (previous-buffer)
    (denote-journal-extras-new-or-existing-entry)))
