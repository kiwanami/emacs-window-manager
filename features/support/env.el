;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq e2wm-root-path project-directory)
  (setq e2wm-util-path (expand-file-name "util" e2wm-root-path)))

(add-to-list 'load-path e2wm-root-path)
(add-to-list 'load-path (expand-file-name "espuds" e2wm-util-path))
(add-to-list 'load-path (expand-file-name "ert" e2wm-util-path))

(require 'e2wm)
(require 'espuds)
(require 'ert)


(Setup
 ;; Before anything has run
 (setq e2wm:c-recordable-buffer-p
       (lambda (buf)
         (e2wm:aand (buffer-name buf)
                    (string-prefix-p "recordable" it t))))
 (e2wm:stop-management t))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
