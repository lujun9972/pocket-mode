;;; pocket.el --- Manage your pocket

;; Copyright (C) 2004-2016 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-5-23
;; Version: 0.1
;; Keywords: convenience, pocket
;; Package-Requires: ((emacs "24.4") (pocket-api "0.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; clean-buffers's code can be found here:
;;   http://github.com/lujun9972/pocket-mode

;; Quick start:

;; execute the following commands:
;; `list-pocket' to list posts in pocket.com
;; then press ~v~ to view post in eww
;; press ~<RET>~ or ~click down mouse-1~ to browse current post with external browser
;; press ~<next>~ to list posts in next-page
;; press ~<prior>~ to list posts in previous-page
;; Press ~a~ to archive the post
;; Press ~C-u a~ to readd the post
;; Press ~d~ to delete the post
;; Press ~C-u d~ to add the post

;;; Code:

(require 'cl-lib)
(require 'pocket-api)

(defvar pocket-current-item 1)
(defvar pocket-items-per-page 10)

(cl-defun pocket-retrieve (&key (offset pocket-current-item) (count pocket-items-per-page))
  "Retrieve pocket items"
  (let* ((pocket-response (pocket-api-get :offset offset :count count))
         (item-list (cdr (assoc 'list pocket-response)))
         entries)
    (dolist (item item-list)
      (let ((item-id (cdr (assoc 'item_id item)))
            (entry-data (mapcar (lambda (item-format)
                                  (cdr (assoc-string (car item-format) item)))
                                tabulated-list-format)))
        (push (list item-id (apply #'vector entry-data)) entries)))
    (reverse entries)))

(pocket-retrieve)

(defun pocket--select-or-create-buffer-window (buffer-or-name)
  "若frame中有显示`buffer-or-name'的window,则选中该window,否则创建新window显示该buffer"
  (let ((buf (get-buffer-create buffer-or-name)))
    (unless (get-buffer-window buf)
      (split-window)
      (switch-to-buffer buf))
    (select-window (get-buffer-window buf))))

;; define pocket-mode

(defun pocket--get-current-entry-value (title)
  (let ((pos (cl-position-if (lambda (title-format)
                               (string= title (car title-format)))
                             tabulated-list-format))
        (entry (tabulated-list-get-entry)))
    (elt entry pos)))

(defun pocket-eww-view ()
  (interactive)
  (let ((url (pocket--get-current-entry-value "given_url")))
    (pocket--select-or-create-buffer-window "*eww*")
    (eww-browse-url url)))

(defun pocket-browser-view ()
  (interactive)
  (let* ((url (format "https://getpocket.com/a/read/%s" (tabulated-list-get-id))))
    (browse-url url)))

(defun pocket-archive-or-readd (prefix)
  (interactive "P")
  (if prefix
      (pocket-api-archive (tabulated-list-get-id))
    (pocket-api-readd (tabulated-list-get-id))))

(defun pocket-delete-or-add (prefix)
  (interactive "P")
  (if prefix
      (pocket-api-delete (tabulated-list-get-id))
    (pocket-api-add (read-string "pocket url:"))))

(defun pocket-next-page (&optional N)
  (interactive)
  (let ((N (or N pocket-items-per-page)))
    (setq pocket-current-item (+ pocket-current-item N))
    (condition-case err
        (list-pocket)
      (error (setq pocket-current-item (- pocket-current-item 1))
             (signal (car err) (cdr err))))))

(defun pocket-previous-page (&optional N)
  (interactive)
  (let ((N (or N pocket-items-per-page)))
    (setq pocket-current-item (- pocket-current-item N))
    (when (< pocket-current-item 0)
      (setq pocket-current-item 0))
    (list-pocket)))

;;;###autoload
(define-derived-mode pocket-mode tabulated-list-mode "pocket-mode"
  "mode for viewing pocket.com"
  (when (pocket-api-access-granted-p)
    (pocket-api-authorize))
  (setq tabulated-list-format [("given_title" 60 nil)
                               ("given_url" 60 t)]
        tabulated-list-entries 'pocket-retrieve)
  (tabulated-list-init-header)
  (define-key pocket-mode-map (kbd "v") 'pocket-eww-view)
  (define-key pocket-mode-map (kbd "<RET>") 'pocket-browser-view)
  (define-key pocket-mode-map (kbd "<down-mouse-1>") 'pocket-browser-view)
  (define-key pocket-mode-map (kbd "<next>") 'pocket-next-page)
  (define-key pocket-mode-map (kbd "<prior>") 'pocket-previous-page)
  (define-key pocket-mode-map (kbd "a") 'pocket-archive-or-readd)
  (define-key pocket-mode-map (kbd "d") 'pocket-delete-or-add)
  )

;;;###autoload
(defun list-pocket ()
  "list paper in pocket.com"
  (interactive)
  (switch-to-buffer (get-buffer-create "*pocket*"))
  (pocket-mode)
  (tabulated-list-print t))


(provide 'pocket-view)

;;; pocket.el ends here
