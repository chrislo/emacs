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
description:
---

[{{ page.title }}]({{ page.link }})

" title url)))
    (find-file filename)
    (insert content)
    (goto-char (point-max))))

(defun my/toggle-journal ()
  "Toggle between journal and previous buffer."
  (interactive)
  (if (org-journal-is-journal)
      (previous-buffer)
    (org-journal-new-entry nil)))

(defun my/toggle-todo ()
  "Toggle between todo.org and previous buffer."
  (interactive)
  (let ((todo-file "~/org/todo.org"))
    (if (string= (buffer-file-name) (expand-file-name todo-file))
        (switch-to-buffer (other-buffer))
      (find-file todo-file))))

(defun my/cycle-font-height ()
  "Cycle through font heights"
  (interactive)
  (let* ((heights '(200 240 280))
         (current-height (face-attribute 'default :height))
         (next-height (or (cadr (member current-height heights))
                         (car heights))))
    (set-face-attribute 'default nil :height next-height)))
