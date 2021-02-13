(load-file "notes.el")
(load-file "utils.el")

;; Variables
(defvar work-day-path "~/Documents/work-days/")

(defun find-next-incomplete-task (&optional pos)
  "Finds the next incomplete task after a given buffer position and returns that position"
  (let ((regexp (concat "^\*+ \\(" (mapconcat 'identity (org-todo-words) "\\|") "\\).*")))
    (save-excursion
      (goto-char (or pos (point)))
      (re-search-forward regexp nil t nil)
      (beginning-of-line)
      (point))))

(defun get-subtree (pos)
  (save-excursion
    (goto-char pos)
    (org-mark-subtree)
    (buffer-substring (region-beginning) (region-end))))

(defun start-of-next-line (pos)
  "Returns the position of the start of the next line"
  (save-excursion
    (end-of-line)
    (+ 1 pos)))

(defun get-incomplete-tasks (pos)
  (let ((next-pos (find-next-incomplete-task pos)))
    (if (<= next-pos pos)
        nil
      (concat (get-subtree next-pos) (get-incomplete-tasks (start-of-next-line next-pos))))))

(defun yesterdays-incomplete-tasks ()
  (with-temp-buffer
    (insert-file-contents (concat work-day-path (last-day-date) ".org"))
    (get-incomplete-tasks 0)))

;; Writing to the new buffer

(defun write-intro ()
  (insert-org-title-next-level
   (concat "Today is " (propertize (current-date) 'font-lock-face '(:foreground "orange"))))
  (insert-line "Good morning, today is " (format-time-string "%A") " the " (day-with-ordinal)))

(defun start-day ()
  (let ((buf (get-buffer-create (concat (current-date) ".org"))))
    ;; Open to the new buffer
    (with-current-buffer buf
      (org-mode)
      (work-mode)
      ;; Insert content into the buffer
      (write-intro)
      (insert-org-title-next-level "Tasks")
      (insert (yesterdays-incomplete-tasks))

      ;; Save buffer to file
      (write-file (concat work-day-path (current-date) ".org"))
      (pop-to-buffer buf))))

(defun create-new-task ()
  (interactive)
  (goto-char 0)
  (re-search-forward "^** Tasks$")
  (end-of-line)
  (insert "\n*** TODO "))

(define-minor-mode work-mode
  "Additional functionality to org-mode for tracking your work through the day"
  :lighter " work"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c t") 'create-new-task)
            map))
