;;; crowi-test.el --- crowi test code.
;;; Commentary:
;; crowi test code.

;;
;;; Code:

(require 'ert)
(require 'crowi)

(ert-deftest crowi-write-to-tmp-file-test ()
  "Test of `crowi-write-to-tmp-file`."
  (should (equal t
				 (crowi-write-to-tmp-file
				  (make-temp-file "crowi" nil ".temp") "test data" ))))

(ert-deftest crowi-create-buffer-test ()
  "Test of `crowi-create-buffer`."
  (should (equal "*Crowi*"
				 (buffer-name(crowi-create-buffer)))))

(defvar crowi-mkdir-test-dir
  (concat (file-name-directory load-file-name) "/t1/t2"))

(ert-deftest crowi-mkdir-parent-test ()
  "Test of `crowi-mkdir-parent`."
  (should (equal t
				 (crowi-mkdir-parent (concat crowi-mkdir-test-dir "/t3"))))
  (when (file-directory-p crowi-mkdir-test-dir)
	(delete-directory crowi-mkdir-test-dir)
	(delete-directory (file-name-directory crowi-mkdir-test-dir))))

(ert-deftest crowi-candidate-page-test ()
  "Test of `crowi-candidate-page`."
  (should (equal t
				 (listp (crowi-candidate "page")))))

(ert-deftest crowi-candidate-path-test ()
  "Test of `crowi-candidate-path`."
  (should (equal t
				 (listp (crowi-candidate "path")))))

(ert-deftest crowi-candidate-basename-test ()
  "Test of `crowi-candidate-basename`."
  (should (equal t
				 (listp (crowi-candidate "basename")))))

(ert-deftest crowi-pageid-test ()
  "Test of `crowi-pageid`."
  (should (equal t
				 (stringp (crowi-pageid (car (crowi-candidate "page")))))))

(ert-deftest crowi-curl-download-test ()
  "Test of `crowi-curl-download`."
  (if (file-exists-p "./crowi-curl-download.test")
	  (delete-file "./crowi-curl-download.test"))
  (should (equal 0
				 (crowi-curl-download crowi-uri "./crowi-curl-download.test")))
  (should (equal t
				 (file-exists-p "./crowi-curl-download.test")))
  (delete-file "./crowi-curl-download.test"))

(ert-deftest crowi-api-test ()
  "Test of `crowi-api`."
  (should (equal t
				 (listp (crowi-api "GET" "/pages.get"
								   (concat "&path="
										   (car (crowi-candidate "page"))))))))

(ert-deftest crowi-users-list-test()
  "Test of `crowi-users-list`."
  (crowi-users-list)
  (switch-to-buffer "*Crowi*")
  (should (equal t
				 (integerp (search-backward crowi-user nil t))))
  (kill-buffer "*Crowi*"))
;;
(ert-deftest crowi-a-pages-create-from-file-test ()
  "Test of `crowi-pages-create-from-file`."
  (if (file-exists-p "./test.md")
	  (delete-file "./test.md"))
  (with-temp-buffer
	(insert  "create")
	(write-file "./test.md"))
  (let ((unread-command-events
		 (listify-key-sequence
		  (kbd "test.md RET /test/ RET crowi-test RET"))))
	(should (equal"/test/crowi-test is create from file"
				  (crowi-pages-create-from-file))))
  (let ((unread-command-events
		 (listify-key-sequence
		  (kbd "/test/crowi-test RET"))))
	(setq crowi-markdown-save nil)
	(crowi-pages-get))
  (switch-to-buffer "*Crowi*")
  (goto-char (point-min))
  (should (equal "create"
				 (buffer-substring-no-properties
				  (point-min) (line-end-position))))
  (kill-buffer "*Crowi*")
)

(ert-deftest crowi-candeidate-test2 ()
  (crowi-test-arrange)
  "Test of `crowi-candidate`."
  (should (member "/test/crowi-test" (crowi-candidate "page")))
  (should (member "/test/" (crowi-candidate "path")))
  (should (member "crowi-test" (crowi-candidate "basename"))))

(ert-deftest crowi-pages-update-test ()
  "Test of `crowi-pages-update-from-file`."
  (crowi-test-arrange)
  (if (file-exists-p "./test.md")
	  (delete-file "./test.md"))
  (with-temp-buffer
	(insert  "update")
	(write-file "./test.md"))
  (let ((unread-command-events
		 (listify-key-sequence
		  (kbd "test.md RET /test/crowi-test RET"))))
	(should (equal "/test/crowi-test is update from file"
				   (crowi-pages-update-from-file))))
  (let ((unread-command-events
		 (listify-key-sequence
		  (kbd "/test/crowi-test RET"))))
	(setq crowi-markdown-save nil)
	(crowi-pages-get))
  (switch-to-buffer "*Crowi*")
  (goto-char (point-min))
  (should (equal "update"
				 (buffer-substring-no-properties
				  (point-min) (line-end-position))))
  (kill-buffer "*Crowi*")
)

(ert-deftest crowi-pages-get-save-test ()
  "Test of `crowi-pages-get` When save."
  (crowi-test-arrange)
  (if (file-exists-p (concat crowi-markdown-save-path "/test/crowi-test.md"))
	  (delete-file (concat crowi-markdown-save-path "/test/crowi-test.md")))
  (setq crowi-markdown-save t)
  (let ((unread-command-events
		 (listify-key-sequence
		  (kbd "/test/crowi-test RET RET"))))
	(crowi-pages-get))
  (should (equal t
				 (file-exists-p
				  (concat crowi-markdown-save-path "/test/crowi-test.md"))))
  (delete-file (concat crowi-markdown-save-path "/test/crowi-test.md"))
	  )

(ert-deftest crowi-comment-add-from-buffer-test ()
  "Test of `crowi-comments-add-from-buffer`."
  (crowi-test-arrange)
  (crowi-create-buffer)
  (switch-to-buffer "*Crowi*")
  (insert  "comments")
  (let ((unread-command-events
		 (listify-key-sequence
		  (kbd "/test/crowi-test RET"))))
	(should (equal "for /test/crowi-test comment is added"
				   (crowi-comments-add-from-buffer))))
  (kill-buffer "*Crowi*")

  "Test of `crowi-comments-get`."
  (crowi-test-arrange)
  (let ((unread-command-events
		 (listify-key-sequence
		  (kbd "/test/crowi-test RET"))))
	(crowi-comments-get))
  (switch-to-buffer "*Crowi*")
  (goto-char (point-min))
  (should (equal "comments"
				 (buffer-substring-no-properties
				  (point-min) (line-end-position))))
  (kill-buffer "*Crowi*")
  )

(ert-deftest crowi-pages-list-test()
  "Test of `crowi-pages-list`."
  (crowi-pages-list)
  (switch-to-buffer "*Crowi*")
  (goto-char (point-min))
  (should (equal (car(crowi-candidate "page"))
				 (buffer-substring-no-properties
				  (point-min) (line-end-position))))
  (kill-buffer "*Crowi*"))

(defun crowi-test-arrange ()
  "Arrange of crowi test."
  (if (file-exists-p "./test.md")
	  (delete-file "./test.md"))
  (with-temp-buffer
	(insert  "create")
	(write-file "./test.md"))
  (let ((unread-command-events
		 (listify-key-sequence
		  (kbd "test.md RET /test/ RET crowi-test RET"))))
	(ignore-errors (crowi-pages-create-from-file))
	(delete-file "./test.md")))

;;api

;;api-attachments

;; create from file

;;candidate

;; pages-list

;; update from file

;; get pages file

;; check list

;; create from buffer

;;comments add

;; comments get

;; attachments add

;;attachments download

;; attachments remove


(provide 'crowi-test)
;;; crowi-test.el ends here
