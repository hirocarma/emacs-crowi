;;; crowi.el --- Emacs client for crowi.

;; Copyright (C) 2017 Hiroshi Sudo

;; Author: Hiroshi Sudo <carma386@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (markdown-mode "2.4"))
;; Keywords: Markdown, crowi
;; URL: https://github.com/hirocarma/emacs-crowi

;;; Commentary:

;; This package assists the user in managing crowi markdown.
;; Please see README.md for documentation.

;;; Code:
(require 'json)
(require 'markdown-mode)

(defgroup crowi nil
  "Elisp client for Crowi."
  :group 'tools
  :prefix "crowi-")

;;; Constant
(defconst crowi-version "0.1.0")
(defconst crowi-api-endpoint "/_api")

;;; Customizable variables
(defcustom crowi-uri "http://localhost:3000"
  "Crowi URI."
  :type 'string)
(defcustom crowi-access-token ""
  "Crowi Access Token."
  :type 'string)
(defcustom crowi-user (getenv "USER")
  "Crowi USER."
  :type 'string)
(defcustom crowi-curl "curl"
  "Executable for curl command."
  :type 'string)
(defcustom crowi-curl-options nil
  "Curl command additional options.  Separate each option with a space."
  :type 'string)
(defcustom crowi-markdown-save nil
  "Whether to save the Get Markdown in file."
  :type 'boolean)
(defcustom crowi-markdown-save-path "~/wiki"
  "Save path for Markdown in file."
  :type 'string)
(defcustom crowi-attachment-save-path "~/wiki"
  "Save path for attachment file."
  :type 'string)

;;; Utility
(defun crowi-write-to-tmp-file (target-path data)
  "Write file to TARGET-PATH from DATA."
  (let ((res (ignore-errors
			   (with-temp-file target-path (insert data) target-path)
			   t)))
	(unless res
	  (error-message-string "failed crowi-write-to-tmp-file"))
	res))

(defun crowi-create-buffer ()
  "Create a buffer to display the result."
  (interactive)
  (kill-buffer (get-buffer-create "*Crowi*"))
  (switch-to-buffer (get-buffer-create "*Crowi*")))

(defun crowi-mkdir-parent (path)
  "Make parent directory as PATH."
  (let* ((pathlist '(""))
	(res (ignore-errors
	(mapc (lambda (i)
			(unless (file-directory-p (mapconcat 'identity pathlist "/"))
			  (make-directory (mapconcat 'identity pathlist "/")))
			(add-to-list 'pathlist i t))
		  (split-string (expand-file-name path) "/"))
	t)))
	(unless res
	  (error-message-string "failed crowi-mkdir-parent"))
	res))

(defun crowi-candidate (args)
  "Return list for candidate.  ARGS as What to create candidate."
  (delete-dups (mapcar (lambda (i)
						 (cond
						  ((equal args "page")
						   (assoc-default 'path i))
						  ((equal args "path")
						   (file-name-directory (assoc-default 'path i)))
						  ((equal args "basename")
						   (file-name-nondirectory (assoc-default 'path i)))
						  (t
						   (assoc-default 'path i))))
					   (assoc-default 'pages (crowi-api "GET" "/pages.list")))))

(defun crowi-pageid (path)
  "Return Crowi pageid.  PATH as Crowi path."
  (assoc-default
   'id (assoc 'page (crowi-api "GET" "/pages.get" (concat "&path=" path)))))

(defun crowi-curl-download (uri filename)
  "Download by curl from URI.  Save as FILENAME."
  (with-temp-buffer
	(let* ((fname (expand-file-name filename))
		   (opt  `("-Ss" "--fail" "--create-dirs" "--output" ,fname ,uri))
		   (opt (if crowi-curl-options
					(append opt (split-string crowi-curl-options " ")) opt))
		   (ret (apply #'call-process crowi-curl nil (current-buffer) nil opt)))
	  (unless (zerop ret)
		(error (message "[Curl ERROR] %s"
				 (buffer-substring-no-properties (point-min) (point-max)))))
	  ret)))

;;; API access function
(defun crowi-api (method api &optional parm &optional alist)
  "Access Crowi api.  METHOD is GET or PUT.  Crowi api path as API.
query strings as PARM.  alist for json as ALIST."
  (with-temp-buffer
	(let* ((uri (concat crowi-uri crowi-api-endpoint api
						"?access_token=" (url-hexify-string crowi-access-token)
						"&user=" crowi-user
						parm))
		   (json (json-encode-alist alist))
		   (crowi-tmp-file (make-temp-file "crowi" nil ".temp"))
		   (ret (crowi-write-to-tmp-file crowi-tmp-file json))
		   (opt  `("-X" ,method
				   "-H" "Content-type: application/json"
				   "-Ss"
				   "--fail"
				   "--data-binary" ,(concat "@" crowi-tmp-file)
				   ,uri))
		   (opt (if crowi-curl-options
					(append opt (split-string crowi-curl-options " ")) opt))
		   (ret (apply #'call-process crowi-curl nil (current-buffer) nil opt))
		   res body)
	  (goto-char (point-min))
	  (unless (zerop ret)
		(error (format "[Curl ERROR] %s"
				 (buffer-substring-no-properties (point-min) (point-max)))))
	  (setq body (replace-regexp-in-string
				  "\n+$" ""
				  (buffer-substring-no-properties (point-min) (point-max))))
	  (unless (eq 0 (length body))
		(setq res (json-read-from-string body))
		(unless (eq t (assoc-default 'ok res))
		  (error (format "[Crowi] %s" (assoc-default 'error res))))
	  res))))

(defun crowi-api-attachments.add (page-id file-path)
  "Access Crowi api for attachments.   Crowi page_id as PAGE-ID.
Attachment file path as FILE-PATH."
  (with-temp-buffer
	(let* ((uri (concat crowi-uri crowi-api-endpoint "/attachments.add"
						"?access_token=" (url-hexify-string crowi-access-token)
						"&user=" crowi-user))
		   (pageid (concat "page_id=" page-id))
		   (file (concat "file=@" file-path))
		   (opt  `("-X" "POST"
				   "-Ss"
				   "--fail"
				   "--form" ,pageid
				   "--form" ,file
				   ,uri))
		   (opt (if crowi-curl-options
					(append opt (split-string crowi-curl-options " ")) opt))
		   (ret (apply #'call-process crowi-curl nil (current-buffer) nil opt))
		   res body)
	  (goto-char (point-min))
	  (unless (zerop ret)
		(error (format "[Curl ERROR] %s"
				 (buffer-substring-no-properties (point-min) (point-max)))))
	  (setq body (replace-regexp-in-string
				  "\n+$" ""
				  (buffer-substring-no-properties (point-min) (point-max))))
	  (unless (eq 0 (length body))
		(setq res (json-read-from-string body))
		(unless (eq t (assoc-default 'ok res))
		  (error (format "[Crowi] %s" (assoc-default 'error res))))
	  res))))

;;; interactive
;; users
;;;###autoload
(defun crowi-users-list ()
  "Output Crowi users list in buffer."
  (interactive)
  (crowi-create-buffer)
  (insert
   (mapconcat 'identity
			  (mapcar (lambda (i) (concat (assoc-default 'name i ) ":"
										  (assoc-default 'username i ) ":"
										  (assoc-default 'email i )))
					  (assoc-default 'users (crowi-api "GET" "/users.list")))
			  "\n" )))

;; pages
;;;###autoload
(defun crowi-pages-list ()
  "Output Crowi pages list in buffer."
  (interactive)
  (crowi-create-buffer)
  (insert
   (mapconcat 'identity
			  (mapcar (lambda (i)
						(assoc-default 'path i))
					  (assoc-default 'pages (crowi-api "GET" "/pages.list")))
			  "\n")))

;;;###autoload
(defun crowi-pages-get ()
  "Output Crowi page."
  (interactive)
  (crowi-create-buffer)
  (let ((path (completing-read "Crowi page ? "
									  (crowi-candidate "page"))))
	(insert (assoc-default
			 'body (assoc 'revision
						  (assoc 'page
								 (crowi-api
								  "GET" "/pages.get" (concat "&path=" path))))))
	(goto-char (point-min))
	(markdown-mode)
	(when crowi-markdown-save
	  (let ((save-path (completing-read "Download path ? "
										(list crowi-markdown-save-path) nil
										'confirm-after-completion)))
		(when (eq 0 (length save-path)
				  (setq save-path crowi-markdown-save-path)))
		(unless (file-directory-p
				 (concat save-path (file-name-directory path))
				 (crowi-mkdir-parent
				  (concat save-path path))))
		(write-file (concat save-path path ".md"))))))

(defun crowi-pages-update (body)
  "Update Crowi page.  BODY as Crowi page for update."
  (let* ((path (completing-read "Crowi update page ? "
										(crowi-candidate "page")))
		 (args `( ("page_id" . ,(crowi-pageid path))("body" . ,body))))
	(assoc-default 'path
				   (assoc 'page (crowi-api "POST" "/pages.update" nil args)))))
;;;###autoload
(defun crowi-pages-update-from-buffer ()
  "Update Crowi page from buffer contents."
  (interactive)
  (message "%s is update from buffer"
		   (crowi-pages-update
			(buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun crowi-pages-update-from-file ()
  "Update Crowi page from file."
  (interactive)
  (let ((file-name (read-file-name "Markdown file for update?" nil nil t)))
  (message "%s is update from file"
		   (crowi-pages-update (with-temp-buffer
								 (insert-file-contents file-name)
								 (buffer-substring-no-properties
								  (point-min) (point-max)))))))

(defun crowi-pages-create (body)
  "Create Crowi page.  BODY as Crowi page for create."
  (let* ((path
		  (completing-read "Crowi parent path for create ? "
						   (cons (concat "/user/" crowi-user "/memo/"
										 (format-time-string "%Y/%m/%d/"
															 (current-time)))
								 (crowi-candidate "path"))
						   nil 'confirm-after-completion))
		 (path (if (equal (substring path -1) "/") path (concat path "/")))
		 (basename
		  (completing-read "Crowi page name for create? "
									(if (buffer-file-name)
										(cons
										 (file-name-base (buffer-file-name))
											  (crowi-candidate "basename"))
									  (crowi-candidate "basename"))
									nil 'confirm-after-completion ))
		 (path (concat path basename))
		 (args `( ("path" . ,path)("body" . ,body))))
	(assoc-default 'path
				   (assoc 'page (crowi-api "POST" "/pages.create" nil args)))))
;;;###autoload
(defun crowi-pages-create-from-buffer ()
  "Create Crowi page from buffer."
  (interactive)
  (message "%s is create from buffer"
		   (crowi-pages-create
			(buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun crowi-pages-create-from-file ()
  "Create Crowi page from file."
  (interactive)
  (let ((file-name (read-file-name "Markdown file for create?" nil nil t)))
  (message "%s is create from file"
  (crowi-pages-create (with-temp-buffer
					 (insert-file-contents file-name)
					 (buffer-substring-no-properties
					  (point-min) (point-max)))))))

;; comments
;;;###autoload
(defun crowi-comments-get ()
  "Get Crowi page comments in buffer."
  (interactive)
  (crowi-create-buffer)
  (let* ((path (completing-read "Crowi page for get comments? "
									  (crowi-candidate "page")))
		 (data (crowi-api "GET" "/comments.get"
						  (concat "&page_id=" (crowi-pageid path)))))
	(when (eq 0 (length (assoc-default 'comments data)))
	  (error (format "No comments on %s" path)))
	(insert
	 (mapconcat 'identity
				(mapcar (lambda (i)
						  (assoc-default 'comment i ))
						(assoc-default 'comments data)) "\n"))
	(goto-char (point-min))))

;;;###autoload
(defun crowi-comments-add-from-buffer ()
  "Add Crowi comment from buffer contents."
  (interactive)
  (let* ((path (completing-read "What is Crowi page to comment on? "
									  (crowi-candidate "page")))
		 (data (crowi-api "GET" "/pages.get" (concat "&path=" path)))
		 (pageid (assoc-default 'id (assoc 'page data)))
		 (revisionid (assoc-default '_id (assoc-default
										  'revision (assoc 'page data))))
		 (comment (buffer-substring-no-properties (point-min) (point-max)))
		 (args `(("commentForm" . (("page_id" . ,pageid)("comment" . ,comment)
								   ("revision_id" . ,revisionid))))))
	(crowi-api "POST" "/comments.add" nil args)
	(message "for %s comment is added" path)))

;; attachments
;;;###autoload
(defun crowi-attachments-download ()
  "Download Crowi attachments."
  (interactive)
  (let* ((path (completing-read "Crowi page for download attachments? "
									  (crowi-candidate "page")))
		 (data (crowi-api "GET" "/attachments.list"
						  (concat "&page_id=" (crowi-pageid path)))))
	(when (eq 0 (length (assoc-default 'attachments data)))
	  (error (format "No attachments on %s" path)))
	(let* ((save-path (completing-read "Download path ? "
									   (list crowi-attachment-save-path) nil
									   'confirm-after-completion)))
	  (when (eq 0 (length save-path)
				(setq save-path crowi-markdown-save-path)))
	  (mapc (lambda (i)
			  (crowi-curl-download
			   (concat crowi-uri (assoc-default 'url i))
			   (concat save-path path "_" (assoc-default 'originalName i))))
			(assoc-default 'attachments data))
	  (message "%s attachments has been downloaded." path))))

(defun crowi-attachments-add ()
  "Add Crowi attachment from file."
  (interactive)
  (let* ((path (completing-read "Crowi attachment page ? "
									  (crowi-candidate "page")))
		 (file-name (read-file-name "attachment file?" nil nil t)))
	(crowi-api-attachments.add (crowi-pageid path) file-name)
	(message "%s" (concat file-name " attached to" path))))

(defun crowi-attachments-remove ()
  "Remove Crowi attachments."
  (interactive)
  (let* ((path (completing-read "Crowi page for remove attachments? "
									  (crowi-candidate "page")))
		 (data (crowi-api "GET" "/attachments.list"
						  (concat "&page_id=" (crowi-pageid path)))))
	(when (eq 0 (length (assoc-default 'attachments data)))
	  (error (format "No attachments on %s" path)))
	(let* ((file (completing-read "remove file ? "
	  (mapcar (lambda (i)
			   (assoc-default 'originalName i))
			  (assoc-default 'attachments data)) nil )))
	  (mapc (lambda (i)
			   (if (equal file (assoc-default 'originalName i))
				   (crowi-api "POST" "/attachments.remove" nil
				   `(("attachment_id" . ,(assoc-default '_id i))))))
			  (assoc-default 'attachments data))
	  (message "attachment %s  has been removed." file))))


(provide 'crowi)

;;; crowi.el ends here
