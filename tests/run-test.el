;;; run-test.el  --- run test crowi.el
;;; Commentary:
;; You must set var for test in crowi-test-env.el.
;; Example:
;; (setq crowi-access-token "xxxxx")
;; (setq crowi-user (getenv "USER"))
;; (setq crowi-uri "https://hoge.net")
;; (setq crowi-curl "curl")
;; (setq crowi-curl-options "some options")
;;
;; (setq crowi-markdown-save t)
;; (setq crowi-markdown-save-path "/somewhere")
;; And run `make`.


;;; Code:

(defvar crowi-test-dir (file-name-directory load-file-name))
(defvar crowi-root-dir (concat crowi-test-dir ".."))

;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
	  (list crowi-test-dir
			crowi-root-dir))

;; Load tests
(load "crowi-test-env")
(load "crowi-test")

;; Run tests
(if noninteractive
	(ert-run-tests-batch-and-exit)
  (ert t))

(provide 'run-test)
;;; run-test.el ends here
