(require 'timer)

(defvar notes/db-path "~/Documents/work/notes/db.org")
(defvar notes/buffer-name " notes/db")
(defvar notes/buffer nil)

(run-with-timer 30 30 (lambda ()
                        (if (buffer-modified-p notes/buffer)
                            (notes/save-db))))

;; Notes project
(defun notes/create-db ()
  "Create the db based on a template"
  (with-temp-buffer
    (insert "* Tags\n* Projects")
    (write-file notes/db-path)))

(defun notes/save-db ()
  "Save the db to a file"
  (with-current-buffer notes/buffer
    (write-file notes/db-path)))


(defun notes/load-db ()
  "This will load up the notes db"
  (let ((buf (get-buffer-create notes/buffer-name)))
    (if (not (file-exists-p notes/db-path))
        (notes/create-db))

    (with-current-buffer buf
      (insert-file-contents notes/db-path)
      (setq notes/buffer buf))))

(defun find-in-org-buffer (value &optional exact)
  "Find a string in the current buffer and returns the position"
  (save-excursion
    (goto-char 0) ; Start search at the beginning of the buffer
    (re-search-forward
     (if exact
         value
       (concat "^\*+.*" value ".*"))
     nil t nil)))

(defun notes/find-project (proj-name)
  "Returns the position of a project"
  (with-current-buffer notes/buffer
    (save-excursion
      (goto-char (find-in-org-buffer "* Projects$" t))
      (find-in-org-buffer (concat "^** " proj-name ".*"))))) ; .* allows for tags

(defun notes/get-project (proj-name)
  "Returns the contents of project proj-name or nil if it doesn't exist"
  (with-current-buffer notes/buffer
    (save-excursion
      (let ((proj-pos (notes/find-project proj-name)))
        (if proj-pos
            (get-subtree proj-pos)
          nil)))))

(defun notes/create-project (proj-name &optional tags)
  "Creates a new project entry with the name PROJ-NAME"
  (if (notes/get-project proj-name)
      nil
    (with-current-buffer notes/buffer
      (goto-char (find-in-org-buffer "^* Projects$" t))
      (end-of-line)
      (insert (concat "\n** " proj-name))
      (if tags
          (org-set-tags tags)))))

(defun notes/read-project-name (&optional previous-failure)
  "Requests a project name from the user, asks again if one was not found"
  (let* ((message (if previous-failure
                     "That project does not exist!\nTry again: "
                    "Project name: "))
         (proj-name (read-string message)))
    (if (notes/find-project proj-name)
        proj-name
      (if (y-or-n-p "That project doesn't exist, do you want to create it?")
          (progn
            (notes/create-project proj-name)
            proj-name)
        (notes/read-project-name t)))))

(defun notes/read-tags ()
  (let ((tag-string (read-string "Tags (comma delimited): ")))
    (if (string-empty-p tag-string)
        nil
      (split-string tag-string "[,]" t "[\s]+"))))

(defun notes/create-entry-from-buffer ()
  "Display a new buffer for the user to input their notes"
  (interactive)
  (let ((buf (get-buffer-create "notes/input")))
    (with-current-buffer buf
      (insert "# Insert note for this entry.\n")
      (insert "# Finish with C-c C-c, or cancel with C-c C-k.\n")
      (org-mode)
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key (kbd "C-c C-c") (lambda ()
                                       (interactive)
                                       (goto-char 0)
                                       (next-line 2)
                                       (beginning-of-line)
                                       (notes/create-entry (buffer-substring (point) (point-max)))
                                       (kill-this-buffer)))
      (local-set-key (kbd "C-c C-k") 'kill-this-buffer)
      (pop-to-buffer buf))))

(defun notes/create-entry (entry &optional proj-name title tags)
  "Adds a new notes entry to the given project (user is prompted if PROJ-NAME is nil"
  (let* ((project-name (or proj-name (notes/read-project-name)))
         (project-pos (notes/find-project project-name))
         (title (or title (read-string "Title: ")))
         (tags (or tags (notes/read-tags))))
    (with-current-buffer notes/buffer
      (save-excursion
        (goto-char project-pos)
        (next-line)
        (beginning-of-line)
        (insert (concat "*** " title "\n"))
        (if tags
            (org-set-tags tags))
        (insert (concat entry "\n"))))))

(defun notes/create-entry-from-region ()
  "Creates a new notes entry from the selected region"
  (let ((entry (buffer-substring (region-beginning) (region-end)))
        (proj-name (notes/read-project-name))
        (title (read-string "Title: "))
        (tags (notes/read-tags)))
    (notes/create-entry entry proj-name title tags)))

;; pulling up notes

(defun notes/find-all (content term &optional initial-pos)
  "Returns a list of all matched values in content (uses re-search-forward"
  (goto-char (or initial-pos 0))
  (let ((next-pos (find-in-org-buffer term)))
    (if next-pos
        (progn
          (message next-pos)
          (append
          '((buffer-substring (beginning-of-line) (end-of-line)))
          (notes/find-all content term (1+ (end-of-line)))))
      nil)))

(defun notes/project-list ()
  "Returns the list of projects"
  (with-current-buffer notes/buffer
    (goto-char 0)
    (let ((content (get-subtree (find-in-org-buffer "^* Projects$" t))))
      (notes/find-all content "^\\*\\*.*$"))))
