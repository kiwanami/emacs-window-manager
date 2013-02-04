(require 'e2wm)


;;; Symmetric two column perspective
;; A variant of TWO perspective that has lesser asymmetric behavior.

(e2wm:pst-class-register
 (make-e2wm:$pst-class
  :name   'stwo
  :extend 'two
  :title  "Symmetric Two Columns"
  :switch 'e2wm:dp-stwo-switch
  :popup  'e2wm:dp-stwo-popup))

(defun e2wm:dp-stwo-switch (buf)
  (e2wm:message "#DP STWO switch : %s" buf)
  (let ((wname (wlf:get-window-name (e2wm:pst-get-wm) (selected-window))))
    (cond
     ((memq wname '(left right))
      (e2wm:pst-buffer-set wname buf)
      (e2wm:pst-update-windows)
      t)
     (t nil))))

(defun e2wm:dp-stwo-popup (buf)
  (e2wm:message "#DP STWO popup : %s" buf)
  (cond
   ((e2wm:document-buffer-p buf)        ; document goes to sub
    (e2wm:dp-two-popup-sub buf))
   ((e2wm:history-recordable-p buf)   ; recordable goes to left or sub
    (case (wlf:get-window-name (e2wm:pst-get-wm)
                               (selected-window))
      (left (e2wm:pst-buffer-set 'right buf t t))
      (t    (e2wm:pst-buffer-set 'left  buf t t)))
    (e2wm:pst-update-windows))
   (t (e2wm:dp-two-popup-sub buf)))
  t)
