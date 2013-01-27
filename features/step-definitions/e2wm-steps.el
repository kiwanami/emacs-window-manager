;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I enabled e2wm$"
       (lambda ()
         (e2wm:start-management)))

(When "^I switch to \"\\(.+\\)\" perspective$"
      (lambda (pst)
        (let ((pst-starter (intern (format "e2wm:dp-%s" pst))))
          (funcall pst-starter))))

(Then "^I should\\( not\\|\\) see window \"\\(.+\\)\"$"
      (lambda (not window)
        (let ((not-p (equal not " not"))
              (has-window-p
               (ignore-errors (wlf:get-window (e2wm:pst-get-wm)
                                              (intern window)))))
          (assert
           (if not-p (not has-window-p) has-window-p)
           nil
           "There should be%s window named %s"
           (if not-p " no" "")
           window))))
