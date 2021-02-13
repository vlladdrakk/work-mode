;; utility functions
(defun current-date ()
  (format-time-string "%Y-%m-%d"))

(defun offset-date (offset)
  (let ((yesterday (decode-time)))
    (cl-incf (nth 3 yesterday) offset)
    (format-time-string "%Y-%m-%d" (apply #'encode-time yesterday))))

(defun last-day-date (&optional offset-in)
  "Returns the date of the last logged work day"
  (let ((offset (or offset-in -1)))
    (if (file-exists-p (concat work-day-path (offset-date offset) ".org"))
        (offset-date offset)
      (last-day-date (- offset 1)))))

(defun day-with-ordinal ()
  (let* ((day (string-to-number (format-time-string "%d")))
         (last-digit (% day 10)))
    (format
     (concat
      "%d"
      (if (memq day '(11 12 13)) "th"
        (case last-digit
          (1 "st")
          (2 "nd")
          (3 "rd")
          (otherwise "th"))))
     day)))

(defun get-current-pos-org-level ()
  (save-excursion
    (re-search-backward "^\*+.+" nil t nil)
    (beginning-of-line)
    (thing-at-point 'symbol t)))

(defun insert-org-title (title)
  (insert-line  (concat (get-current-pos-org-level) " " title)))

(defun insert-org-title-next-level (title)
  "Adds a new org title at the next level"
  (insert-line (concat (get-current-pos-org-level) "* " title)))

(defun insert-line (&rest ARGS)
  (apply 'insert  ARGS)
  (insert "\n"))

(defun org-todo-words ()
  "Returns a filtered list of org-todo-keywords"
  (let ((keywords (cdr (car org-todo-keywords))))
    (seq-filter
     (lambda (x)
       (not
        (or
         (equal x "|")
         (equal x "DONE"))))
     keywords)))
