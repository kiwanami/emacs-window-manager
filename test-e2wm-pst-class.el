;; How to run:
;;   emacs -batch \
;;       -L PATH/TO/E2WM/ \
;;       -L PATH/TO/WINDOW-LAYOUT/ \
;;       -l PATH/TO/test-e2wm-pst-class.el \
;;       -f ert-run-tests-batch-and-exit


(require 'ert)
(require 'e2wm)


(ert-deftest e2wm-pst-class-simple-inheritance ()
  (let* ((expected-result 1)
         (super-class
          (make-e2wm:$pst-class :init (lambda () expected-result)))
         (class
          (make-e2wm:$pst-class
           :extend super-class
           :init (lambda () (e2wm:$pst-class-super))))
         (result (e2wm:method-call
                  #'e2wm:$pst-class-init class nil)))
    (should (equal result expected-result))))
