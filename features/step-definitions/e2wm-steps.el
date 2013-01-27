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

(Then "^I should\\( not\\|\\) be in window \"\\(.+\\)\"$"
      (lambda (not window)
        (let* ((not-p (equal not " not"))
               (given-wname (intern window))
               (cur-wname (wlf:get-window-name (e2wm:pst-get-wm)
                                               (selected-window)))
               (same-window-p (eq given-wname cur-wname)))
          (assert
           (if not-p (not same-window-p) same-window-p)
           nil
           "I should%s be in window named %s, but in %s."
           not given-wname cur-wname))))

(And "^I have a popup buffer \"\\(.+\\)\"$"
     (lambda (buffer-name)
       (pop-to-buffer (get-buffer-create buffer-name))))
