;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I enabled e2wm$"
       (lambda ()
         (e2wm:start-management)))

(Then "^I should\\( not\\|\\) be in e2wm-managed frame$"
      (lambda (not)
        (let ((not-p (equal not " not"))
              (pst (e2wm:pst-get-instance)))
          (assert (eq not-p (not pst)) nil
                  "I have frame-local e2wm:pst instance: %S."
                  pst))))

(Then "^I should\\( not\\|\\) be in perspective \"\\(.+\\)\"$"
      (lambda (not desired)
        (let ((not-p (equal not " not"))
              (actual (format "%s" (e2wm:$pst-name (e2wm:pst-get-instance)))))
          (assert (eq (not not-p) (equal actual desired)) nil
                  "Expected%s in perspective %S but in %S."
                  (if not-p " not" "") desired actual))))

(When "^I switch to \"\\(.+\\)\" perspective$"
      (lambda (pst)
        (e2wm:pst-change (intern pst))))

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

(Then "^I should see buffer \"\\(.+\\)\" in window \"\\(.+\\)\"$"
      (lambda (buffer-name window-name)
        (let* ((wm (e2wm:pst-get-wm))
               (actual-buffer (wlf:get-buffer wm (intern window-name))))
          (assert
           (equal buffer-name (buffer-name actual-buffer))
           nil
           "Expected to see buffer %S in window %S but got %S."
           buffer-name window-name (buffer-name actual-buffer)))))

(Then "^I should see these windows:$"
      (lambda (table)
        (let* ((wm (e2wm:pst-get-wm))
               (header (car table))
               (desired-names (sort (mapcar #'car (cdr table))
                                    #'string-lessp))
               (actual-names
                (sort (mapcar
                       (lambda (w) (symbol-name (wlf:get-window-name wm w)))
                       (window-list))
                      #'string-lessp)))
          (assert
           (equal actual-names desired-names)
           nil
           "I have different set of windows: %S"
           actual-names))))

(And "^I have a popup buffer \"\\(.+\\)\"$"
     (lambda (buffer-name)
       (pop-to-buffer (get-buffer-create buffer-name))))

(And "^I switch to a buffer \"\\(.+\\)\"$"
     (lambda (buffer-name)
       (switch-to-buffer (get-buffer-create buffer-name))))

(And "^I switch to window \"\\(.+\\)\"$"
     (lambda (window-name)
       (e2wm:pst-window-select (intern window-name))))

(When "^I switch to window \"\\(.+\\)\" and open buffer \"\\(.+\\)\"$"
      (lambda (window-name buffer-name)
        (When "I switch to window \"%s\"" window-name)
        (And "I switch to a buffer \"%s\"" buffer-name)))

(defun e2wm:testing-separate-table (rows)
  (let ((backup-p t)
        history-backup history)
    (mapc (lambda (row)
            (when (not (equal (car row) ""))
              (setq backup-p nil))
            (if backup-p
                (push (cadr row) history-backup)
              (push (cadr row) history)))
          rows)
    (list history-backup
          (nreverse history))))

(Then "^I should have these buffers in history:$"
      (lambda (table)
        (let* ((header (car table))
               (packed (e2wm:testing-separate-table (cdr table)))
               (given-backup (car packed))
               (given-history (cadr packed))
               (actual-backup (mapcar #'buffer-name (e2wm:history-get-backup)))
               (actual-history (mapcar #'buffer-name (e2wm:history-get))))
          (assert
           (equal given-history actual-history)
           nil
           "I have different history: %S"
           actual-history)
          (assert
           (equal given-backup actual-backup)
           nil
           "I have different history-backup: %S"
           actual-backup))))

(And "^I go forward history$"
     'e2wm:pst-history-forward-command)

(And "^I go back history$"
     'e2wm:pst-history-back-command)

(And "^I execute a command that reopens buffer \"\\(.+\\)\" in other window$"
     (lambda (buffer-name)
       ;; This emulates `vc-follow-link':
       (let ((this-command 'dummy-command))
         (kill-buffer (get-buffer-create buffer-name))
         (switch-to-buffer-other-window (get-buffer-create buffer-name)))))

(Then "^key-binding \"\\(.+\\)\" is undefined$"
      (lambda (key)
        (let ((command (key-binding (edmacro-parse-keys key))))
          (assert (not command) nil
                  "There should be no binding but %s was found."
                  command))))

(Then "^\"\\(.+\\)\" should be called when I type \"\\(.+\\)\"$"
      (lambda (command key)
        (let ((actual (key-binding (edmacro-parse-keys key)))
              (desired (intern command)))
          (assert (eq actual desired) nil
                  "Command %s was bound instead of %s."
                  actual desired))))
