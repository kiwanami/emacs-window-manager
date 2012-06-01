;;; e2wm.el --- simple window manager for emacs

;; Copyright (C) 2010, 2011  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai atmark kiwanami.net>
;; Version: 1.2
;; Keywords: tools, window manager

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an demonstration implementation of introducing window management to Emacs.
;; * Management of list of editable buffers
;; * Assignment of windows for pop-up buffers
;; * Switching window layout like the perspective in eclipse
;; * Plug-in extension 

;; The current implementation has following perspectives:
;; * code      : main coding layout
;; * two       : side by side layout
;; * doc       : reading documentation layout
;; * dashboard : showing plug-ins like dashboard in Mac OSX
;; * array     : selecting buffers like expose in Mac OSX

;;; Installation:

;; (1) Put e2wm.el and window-layout.el in load-path.
;; (2) Put the following code in your .emacs file,
;;    (require 'e2wm)
;; (3) M-x e2wm:start-management to start e2wm.
;; To stop e2wm, M-x e2wm:stop-management or [C-c ; Q].


;;; Customization

;; * Layout recipe (`e2wm:c-PST-NAME-recipe'):

;; Layout recipe RECIPE (e.g., `e2wm:c-code-recipe') is a recursive
;; tree-like structure defined as follows:

;; (SPLIT-TYPE [SPLIT-OPTION]
;;             WINDOW-or-RECIPE    ; left or upper side
;;             WINDOW-or-RECIPE)   ; right or lower side

;; WINDOW is a name (symbol) of the window. This is used in the
;; `:name' property of the window information list (winfo, see the
;; next section).

;; Split types (SPLIT-TYPE):

;;   - : split vertically
;;   | : split horizontally

;; Split option list SPLIT-OPTION is a plist with the following
;; properties. (the prefix 'left' can be replaced by 'right', 'upper'
;; and 'lower'.):

;;   :left-size        : (column or row number) window size
;;   :left-max-size    : (column or row number) if window size is larger
;;                     : than this value, the window is shrunken.
;;   :left-size-ratio  : (0.0 - 1.0) window size ratio. the size of
;;                     : the other side is the rest.
;;
;; Note:
;; The split option can be omitted.
;; The size parameters, :size, :max-size and :size-ratio, are mutually
;; exclusive.  The size of a window is related with one of the other
;; side window. So, if both side windows set size parameters, the
;; window size may not be adjusted as you write.

;; * Window information  (`e2wm:c-PST-NAME-winfo'):

;; Window information (e.g., `e2wm:c-code-winfo') is a list of window
;; options (plist).  Besides the options defined in window-layout.el,
;; `:name' (mandatory), `:buffer', `:default-hide' and `:fix-size',
;; e2wm has additional options.

;;   :name      [*] : the window name.
;;   :buffer        : A buffer name or a buffer object to show the window.
;;                  : If nil or omitted, the current buffer remains.
;;   :default-hide  : If t, the window is hided initially.
;;                  : (type: t or nil, default: nil)
;;   :fix-size      : If t, when the windows are laid out again, the
;;                  : window size is remained.
;;                  : (type: t or nil, default: nil)
;;   :plugin        : Plug-in name.
;;                  : (type: symbol)
;;   :plugin-args   : Arguments for the plug-in.  See each plug-in
;;                  : documentation for use of this option.
;;                  : (type: any lisp objecct)

;;; Development memo:

;; See readme for further documentation.

;; ** Side effects
;; 
;; * advice 
;;  - buffer系
;;     switch-to-buffer, pop-to-buffer
;;  - window-configuration系
;;     current-window-configuration
;;     window-configuration-frame
;;     compare-window-configurations
;;     set-window-configuration
;;     window-configuration-p
;; * hook
;;     kill-buffer-hook
;;     window-configuration-change-hook
;;     completion-setup-hook
;;     after-save-hook
;; * override variable
;;     special-display-function

;; ** Local words
;; pst     : Perspective
;; e2wm:c- : Configuration variables
;; e2wm:$  : Structure functions

;; ** Source code layout

;; Configurations  / e2wm:c-
;; Fundamental functions
;; Buffer history management / e2wm:history-
;; Framework for perspectives / e2wm:pst-
;; Framework for perspective set / e2wm:pstset-
;; Advices and hooks (switch-to-buffer, pop-to-buffer and so on)
;; Framework for plug-ins / e2wm:plugin-
;; Menu definition / e2wm:menu-
;; Plug-in definitions / e2wm:def-plugin-
;; Perspective definitions / e2wm:dp-
;;   code  / e2wm:dp-code-
;;   doc   / e2wm:dp-doc-
;;   two   / e2wm:dp-two-
;;   dashboard / e2wm:dp-dashboard-
;;   array / e2wm:dp-array-
;; Start-up and exit e2wm

;;; Code:

(require 'cl)

(require 'imenu)
(require 'easymenu)
(require 'windmove)

(require 'window-layout)

(eval-when-compile (defvar prev-selected-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Customize
(defvar e2wm:c-max-history-num 20 "Number of buffer history.")
(defvar e2wm:c-recordable-buffer-p
  (lambda (buf)
    (buffer-local-value 'buffer-file-name buf))
  "Return non-nil, if the buffer is an editable buffer.")
(defvar e2wm:c-document-buffer-p ; 
  (lambda (buf)
    (string-match "\\*\\(Help\\|info\\|w3m\\|WoMan\\)" (buffer-name buf)))
  "Retrun non-nil, if the buffer is a document buffer.")
(defvar e2wm:c-blank-buffer
      (let ((buf (get-buffer-create " *e2wm:blank*")))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (buffer-disable-undo buf)
          (erase-buffer)
          (setq buffer-read-only t)) buf)
      "Blank buffer.")

(defvar e2wm:prefix-key "C-c ; " "Prefix key")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Macro / Utilities

(defmacro e2wm:aif (test-form then-form &rest else-forms)
  "Anaphoric IF."
  (declare (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'e2wm:aif 'lisp-indent-function 2)

(defmacro e2wm:aand (test &rest rest)
  "Anaphoric AND."
  (declare (debug (form &rest form)))
  `(let ((it ,test))
     (if it ,(if rest (macroexpand-all `(e2wm:aand ,@rest)) 'it))))

(defmacro e2wm:not-minibuffer (&rest body)
  "Evaluate a body form when the minibuffer is not active."
  (declare (debug (&rest form)))
  `(when (= 0 (minibuffer-depth))
     ,@body))

(defmacro e2wm:safe-call (method object &rest args)
  "Safely method calling."
  (let ((sym (gensym)))
    `(let ((,sym (,method ,object)))
       (if ,sym
           (funcall ,sym ,@args)))))

;; for a list of structure

(defun e2wm:find (name name-func seq)
  "Return the element that the return value of the NAME-FUNC
equals to NAME in the given sequence SEQ."
  (loop for i in seq
        if (eq name (funcall name-func i))
        return i))

(defmacro e2wm:delete! (name name-func seq)
  "Destructively delete the element."
  `(setq ,seq 
         (delete-if 
          (lambda (i) (eq ,name (funcall ',name-func i)))
             ,seq)))

;; debug

(eval-and-compile
  (defvar e2wm:debug nil "Debug output switch.")) ; debug
(defvar e2wm:debug-count 0 "[internal] Debug output counter.") ; debug

(defmacro e2wm:message (&rest args) ; debug
  "Output a message into the debug buffer: *e2wm:debug*."
  (when e2wm:debug
    `(progn 
       (with-current-buffer (get-buffer-create "*e2wm:debug*")
         (save-excursion
           (goto-char (point-max))
           (insert (format "%5i %s\n" e2wm:debug-count (format ,@args)))))
       (incf e2wm:debug-count))))

(defun e2wm:message-mark () ; debug
  "Output a mark text into the debug buffer: *e2wm:debug*."
  (interactive)
  (e2wm:message "==================== mark ==== %s" 
               (format-time-string "%H:%M:%S" (current-time))))

;; keymap

(defun e2wm:define-keymap (keymap-list &optional prefix)
  "[utility] Return a keymap object with given keymap definitions
that is a list of cons cells ([keyboard macro string]
. [function])."
  (let ((map (make-sparse-keymap)))
    (mapc 
     (lambda (i)
       (define-key map
         (if (stringp (car i))
             (read-kbd-macro 
              (if prefix 
                  (replace-regexp-in-string "prefix" prefix (car i))
                (car i)))
           (car i))
         (cdr i)))
     keymap-list)
    map))

(defun e2wm:add-keymap (keymap keymap-list &optional prefix)
  "[utility] Add given keymap definitions to KEYMAP."
  (mapc 
   (lambda (i)
     (define-key keymap
       (if (stringp (car i))
           (read-kbd-macro 
            (if prefix 
                (replace-regexp-in-string "prefix" prefix (car i))
              (car i)))
         (car i))
       (cdr i)))
   keymap-list)
  keymap)

;; window overriding

(defvar e2wm:ad-now-overriding nil
  "[internal] Recursive execution flag. If e2wm is evaluating
overriding functions, this variable is set t.")

(defmacro e2wm:with-advice (&rest body)
  "[internal] Avoid infinite recursion in the overriding
functions, such as `switch-to-buffer' and `pop-to-buffer'.
If the original function should be called, use this macro."
  (declare (debug (&rest form)))
  `(let ((e2wm:ad-now-overriding t))
     ,@body))

;; text / string

(defun e2wm:string-trim (txt)
  "Remove white space characters at head and tail
from the given string."
  (let ((ret txt))
    (setq ret (if (string-match "^\\s-*" ret)
                  (substring ret (match-end 0))
                ret))
    (or
     (loop for i downfrom (1- (length ret)) downto 0 do 
           (if (/= 32 (char-syntax (aref ret i)))
               (return (substring ret 0 (1+ i)))))
     "")))

(defun e2wm:strtime (time)
  "[utility] Format time object."
  (if (equal (cdddr (decode-time time))
             (cdddr (decode-time (current-time))))
      (format-time-string "Today  %H:%M:%S" time)
    (format-time-string   "%Y/%m/%d %H:%M:%S" time)))

(defface e2wm:face-title 
  '((((class color) (background light))
     :foreground "Deeppink2" :height 1.5 :inherit variable-pitch)
    (((class color) (background dark))
     :foreground "yellow" :weight bold :height 1.5 :inherit variable-pitch)
    (t :height 1.5 :weight bold :inherit variable-pitch))
  "Face for e2wm titles at level 1."
  :group 'e2wm)

(defface e2wm:face-subtitle
  '((((class color) (background light))
     (:foreground "Gray10" :height 1.2 :inherit variable-pitch))
    (((class color) (background dark))
     (:foreground "Gray90" :height 1.2 :inherit variable-pitch))
    (t :height 1.2 :inherit variable-pitch))
  "Face for e2wm titles at level 2."
  :group 'e2wm)

(defface e2wm:face-item
  '((t :inherit variable-pitch :foreground "DarkSlateBlue"))
  "Face for e2wm items."
  :group 'e2wm)

(defun e2wm:rt (text face)
  "[utility] Put the face property to TEXT."
  (unless (stringp text) (setq text (format "%s" text)))
  (put-text-property 0 (length text) 'face face text) text)

(defun e2wm:rt-format (text &rest args)
  "[utility] Format strings with faces. TEXT is format
string. ARGS is a list of cons cell, ([string] . [face name])."
  (apply 'format (e2wm:rt text 'e2wm:face-item)
         (loop for i in args
               if (consp i)
               collect (e2wm:rt (car i) (cdr i))
               else
               collect (e2wm:rt i 'e2wm:face-subtitle))))

(defun e2wm:tp (text prop value)
  "[utility] Put a text property to the first character of TEXT."
  (if (< 0 (length text))
      (put-text-property 0 1 prop value text))
  text)

(defun e2wm:format-byte-unit (size)
  "[utility] Format a number with the human readable unit."
  (cond ((null size) "NA")
        ((> size (* 1048576 4))
         (format "%s Mb" (e2wm:num (round (/ size 1048576)))))
        ((> size (* 1024 4))
         (format "%s Kb" (e2wm:num (round (/ size 1024)))))
        (t
         (format "%s bytes" (e2wm:num size)))))

(defun e2wm:num (number)
  "[utility] Format a number."
  (let ((base (format "%s" number)))
    (flet 
        ((rec (str len)
              (let ((pos (- len 3)))
                (if (< pos 1) str
                  (concat (rec (substring str 0 pos) pos)
                          "," (substring str pos))))))
      (rec base (length base)))))

(defun e2wm:max-length (rows)
  "[utility] Return the max length of string in the given
sequence. ROWS is a list of string."
  (loop for i in rows
        with lmax = 0
        for ln = (if i (length i) 0)
        do (setq lmax (max lmax ln))
        finally return lmax))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Base API / History Management

(defun e2wm:frame-param-get (name &optional frame)
  "[internal] Return a value from frame properties."
  (frame-parameter (or frame (selected-frame)) name))

(defun e2wm:frame-param-set (name val &optional frame)
  "[internal] Set a frame property."
  (set-frame-parameter (or frame (selected-frame)) name val))

(defun e2wm:document-buffer-p (buffer)
  "If BUFFER is document, return t. See the variable `e2wm:c-document-buffer-p'."
  (if (and buffer (buffer-live-p buffer))
      (funcall e2wm:c-document-buffer-p buffer)))

;; History data structure
;; 
;;           1: buffer a  |
;;           2: buffer b  | history-backup
;; currently displayed
;;        -> 3: buffer c  | 
;;           4: buffer d  | 
;;           5: buffer e  | history
;; 

(defun e2wm:history-get ()
  "Return a list of buffer history from the current frame."
  (e2wm:frame-param-get 'e2wm:buffer-history))

(defun e2wm:history-save (buffer-history)
  "Save the given list as buffer history in the current frame."
  (e2wm:frame-param-set
   'e2wm:buffer-history buffer-history)
  buffer-history)

(defun e2wm:history-get-backup ()
  "Return a list of buffer backup-history."
  (e2wm:frame-param-get
   'e2wm:buffer-history-backup))

(defun e2wm:history-save-backup (buffer-history-backup)
  "Save the given list as buffer backup-history."
  (e2wm:frame-param-set 
   'e2wm:buffer-history-backup
   buffer-history-backup)
  buffer-history-backup)

(defun e2wm:history-recordable-p (buffer)
  "If BUFFER should be record in buffer history, return t.
See the variable `e2wm:c-recordable-buffer-p'."
  (if (and buffer (buffer-live-p buffer))
      (funcall e2wm:c-recordable-buffer-p buffer)))

(defun e2wm:history-add (buffer)
  "Add BUFFER to buffer history.
This function does following jobs:
* clear all dead buffers
* sort the list of buffer history by LRU
* add a buffer and truncate the tail element
* save the buffer history."
  (e2wm:message "#HISTORY-ADD : %s" buffer)
  (e2wm:aif (get-buffer buffer)
      (let* ((prev-history (e2wm:history-get))
             (last-buffer (car prev-history))
             (history
             (mapcar 
               'car
               (sort 
                (loop for h in (append
                                (cdr prev-history)
                                (e2wm:history-get-backup))
                     for b = (get-buffer h)
                     if (and b (buffer-live-p b))
                     collect (cons b (float-time 
                                      (buffer-local-value 
                                       'buffer-display-time b))))
               (lambda (i j) 
                 (> (cdr i) (cdr j)))))))
        (when last-buffer
          (setq history (cons last-buffer history)))
        (when (e2wm:history-recordable-p it)
          (e2wm:history-save-backup nil)
          (setq history
                (cons it 
                      (if (member it history)
                          (remove it history)
                        (if (< e2wm:c-max-history-num (length history))
                            (nbutlast history) history))))
          (e2wm:history-save history)))))

(defun e2wm:history-back ()
  "Move backward in the buffer history. This function does not update windows."
  (let ((history (e2wm:history-get))
        (history-backup (e2wm:history-get-backup)))
    (when (and history (cdr history))
      (push (pop history) history-backup))
    (e2wm:history-save history)
    (e2wm:history-save-backup history-backup)))

(defun e2wm:history-forward ()
  "Move forward in the buffer history. This function does not update windows."
  (let ((history (e2wm:history-get))
        (history-backup (e2wm:history-get-backup)))
    (when history-backup
      (push (pop history-backup) history))
    (e2wm:history-save history)
    (e2wm:history-save-backup history-backup)))

(defun e2wm:history-delete (buffer)
  "Delete BUFFER from the buffer history. This function does not update windows."
  (let ((history (e2wm:history-get))
        (history-backup (e2wm:history-get-backup)))
    (setq history (remove buffer history))
    (setq history-backup (remove buffer history-backup))
    (when (and (null history) history-backup)
      (push (pop history-backup) history))
    (e2wm:history-save history)
    (e2wm:history-save-backup history-backup)))

(defun e2wm:history-get-next (buffer)
  "Return the buffer that is previous to BUFFER in the buffer history.
If no buffer is found, return BUFFER."
  (let* ((history (append (reverse (e2wm:history-get-backup))
                          (e2wm:history-get))))
    (or (loop for i in history
              with prev = nil
              do
              (when 
               (eql buffer i) (return prev))
              (setq prev i))
        buffer)))

(defun e2wm:history-get-prev (buffer)
  "Return the buffer that is next to BUFFER in the buffer history.
If no buffer is found, return BUFFER."
  (let* ((history (append (reverse (e2wm:history-get-backup))
                          (e2wm:history-get))))
    (or (loop for i in history
              with found = nil
              do
              (cond 
               (found (return i))
               ((eql buffer i) (setq found t))))
        buffer)))

(defun e2wm:history-get-nearest (buffer n)
  "Return a list of N buffers that is near to the BUFFER but is
*not* the BUFFER. If some buffers are not found, return nil."
  (let* ((history (append (reverse (e2wm:history-get-backup))
                          (e2wm:history-get)))
         (prevs nil)
         (nexts nil))
    (loop for i in history
          for c from 0
          with found = nil
          if (and found (< n c))
          return found
          else
          do (if (eql buffer i)
                 (setq found t)
               (if found
                   (push i nexts)
                 (push i prevs))))
    (if (or prevs nexts)
        (loop for i in (subseq (append prevs (reverse nexts)) 0 n)
              with last-non-nil = nil
              if i collect i and do (setq last-non-nil i)
              else collect last-non-nil)
      nil)))
  
(defun e2wm:history-get-main-buffer ()
  "Return the main buffer that should be display as the current
editing buffer."
  (e2wm:aif (e2wm:history-get)
      (car it) e2wm:c-blank-buffer))

(defun e2wm:managed-p (&optional frame)
  "Return t, if e2wm manages the current frame."
  (e2wm:pst-get-instance frame))

(defun e2wm:internal-buffer-p (buf)
  "Return t, if BUF is internal buffer created by e2wm.
The current implementation check the buffer name. TODO: improve the internal sign."
  (e2wm:aand buf (string-match "\\*WM:" (buffer-name it))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Perspective Framework

(defvar e2wm:pst-list nil "[internal] Perspective class registory.")
(setq e2wm:pst-list nil)

(defvar e2wm:prev-selected-buffer nil
  "The dynamically bound variable pointed to the previous buffer.
This is available in `init', `start' and `leave' function.
Note that `prev-selected-buffer' is obsolete now.")

;; structure  [ e2wm:$pst-class ]
;;
;;   This structure defines a perspective.
;;   The symbol (*) means the slot must be defined.
;;
;; name    : (*) The identifier symbol for this perspective
;; extend  : A symbol for the base perspective class.
;;         : If the functions described below are set nil, the functions of the base class are called.
;;         : Note that the functions can call the dynamically bound function `e2wm:$pst-class-super' to
;;         : evaluate the base class ones.
;; init    : (*) The constructor function for this perspective. (Arguments: none)
;;         : This function just set up the wset structure object and return it.
;;         : Actual laying out and setting up hooks should be done in the `start' function.
;;         : The dynamically bound variable `e2wm:prev-selected-buffer' can be used at the 
;;         : functions `init' and `start'.
;; title   : (*) A string for the title of this perspective.
;; main    : A symbol for the window name which is selected initially.
;;         : If this slot is nil, the e2wm doesn't care of window focus.
;; start   : This function lays out windows and set up buffers and some variables for this perspective.
;;         : (Arguments: `wm')
;;         : If this slot is nil, the e2wm does nothing at the beginning of the perspective.
;;         : This function may be called again, after the evaluating `leave' and
;;         : suspending the e2wm management.
;; update  : This function updates the windows and plug-ins. (Arguments: `wm')
;;         : If this slow it nil, the e2wm just refreshes the windows and plug-ins.
;; switch  : This function overrides `switch-to-buffer'. (Arguments: `buffer')
;;         : If this slot is nil or this function returns nil, 
;;         : the original function `switch-to-buffer' is evaluated.
;;         : If this function returns non-nil, the original function `switch-to-buffer' is not evaluated.
;;         : If the plug-ins need to be updated, this function should call the 
;;         : `e2wm:pst-update-windows' to update the plug-ins.
;; popup   : This function overrides `pop-to-buffer' and `special-display-func'. (Arguments: `buffer')
;;         : See the `switch' spec for detail.
;; leave   : This function cleans up buffers and some variables for leaving the perspective.
;;         : (Arguments: `wm')
;;         : If this slot is nil, the e2wm does nothing during leaving the perspective.
;; keymap  : A symbol for the key map of this perspective.
;; save    : This function is called by the hook `after-save-hook' when this perspective is active.

(defstruct e2wm:$pst-class 
  name title extend
  init main start update switch popup leave
  keymap save)

(defun e2wm:pst-class-register (pst-class)
  "Register a perspective class. If the class which name is
 the same as PST-CLASS has been already registered, the given class
overrides the previous one."
  (when (e2wm:aand (e2wm:$pst-class-extend pst-class)
                  (symbolp it))
    ;; A symbol for the super class is replaced by the class object.
    (setf (e2wm:$pst-class-extend pst-class)
          (e2wm:pst-class-get (e2wm:$pst-class-extend pst-class))))
  (e2wm:pst-class-remove pst-class)
  (push pst-class e2wm:pst-list))

(defun e2wm:pst-class-remove (pst-class)
  "Remove the class from the perspective class registry."
  (setq e2wm:pst-list
        (loop with name = (e2wm:$pst-class-name pst-class)
              for i in e2wm:pst-list
              unless (equal name (e2wm:$pst-class-name i))
              collect i)))

(defun e2wm:pst-class-get (name)
  "Return the class object which name is NAME."
  (e2wm:find name 'e2wm:$pst-class-name e2wm:pst-list))

(defun e2wm:pst-class-abstract-p (pst-class)
  "Return non-`nil' when PST-CLASS does not have all mandatory
slots (i.e., `:init' and `:title')."
  (and pst-class
       (not (e2wm:$pst-class-init pst-class))
       (not (e2wm:$pst-class-title pst-class))))


;; structure [e2wm:$pst] 
;;
;;   This structure represents an instance of the perspective.
;;
;; name    : A symbol for this perspective
;; wm      : wlf layout object
;; type    : A reference to the perspective class object

(defstruct e2wm:$pst name wm type)

(defun e2wm:$pst-get-prop (name pst)
  "[internal] Return the value of this perspective."
  (let ((slot-name (intern (format "e2wm:$pst-class-%s" name))))
    (e2wm:$pst-class-get-prop-gen slot-name (e2wm:$pst-type pst))))

(defun e2wm:$pst-class-get-prop-gen (slot-name pst-class)
  "[internal] Return the slot value of this perspective class."
  (or (funcall slot-name pst-class)
      (e2wm:aif (e2wm:$pst-class-extend pst-class)
          (e2wm:$pst-class-get-prop-gen slot-name it))))

(defun e2wm:method-call (method-name class error-on-nil &rest args)
  "[internal] Call the method which belongs to the perspective class.
If ERROR-ON-NIL is non-nil and the CLASS has no value at the slot, 
raise the error signal with ERROR-ON-NIL."
  (lexical-let ((method (e2wm:$pst-class-get-prop-gen method-name class))
                (super-class (e2wm:$pst-class-extend class))
                ;; put all arguments in lexical scope to use it in
                ;; `e2wm:$pst-class-super':
                (method-name method-name)
                (class class)
                (error-on-nil error-on-nil)
                (args args))
    (cond
     ((null method)
      (if error-on-nil (error error-on-nil) nil))
     (t
      (flet ((e2wm:$pst-class-super () (apply
                                        #'e2wm:method-call
                                        method-name super-class
                                        error-on-nil args)))
        (apply method args))))))

(defmacro e2wm:pst-method-call (method-name pst-instance &rest args)
  "[internal] Short cut macro. (pst-instance -> pst-class)"
  `(e2wm:method-call 
    ',method-name 
    (e2wm:$pst-type ,pst-instance) nil ,@args))

(defun e2wm:$pst-class-super ()
  "Dynamically bound super class method of the current perspective method.

WARNING: Call this function only *inside* of a perspective method
which class (`e2wm:$pst-class') has the `:extend' slot.

This function is a dummy implementation for suppressing compile
warning and providing information to user via docstring and error
message.  Directly calling this function raises error.

See `e2wm:method-call' for implementation."
  (error "e2wm:$pst-class-super is called outside of perspective method."))

(defun e2wm:$pst-title (pst)
  (e2wm:$pst-get-prop 'title pst))
(defun e2wm:$pst-main (pst)
  (e2wm:$pst-get-prop 'main pst))
(defun e2wm:$pst-keymap (pst)
  (e2wm:aif (e2wm:$pst-get-prop 'keymap pst)
      (symbol-value it) nil))

(defun e2wm:$pst-start (pst)
  (e2wm:$pst-class-start (e2wm:$pst-type pst)))
(defun e2wm:$pst-update (pst)
  (e2wm:$pst-class-update (e2wm:$pst-type pst)))
(defun e2wm:$pst-switch (pst)
  (e2wm:$pst-class-switch (e2wm:$pst-type pst)))
(defun e2wm:$pst-popup (pst)
  (e2wm:$pst-class-popup (e2wm:$pst-type pst)))
(defun e2wm:$pst-leave (pst)
  (e2wm:$pst-class-leave (e2wm:$pst-type pst)))
(defun e2wm:$pst-save (pst)
  (e2wm:$pst-class-save (e2wm:$pst-type pst)))
(defun e2wm:$pst-super (pst)
  (e2wm:$pst-class-extend (e2wm:$pst-type pst)))

(defun e2wm:pst-get-instance (&optional frame)
  (e2wm:frame-param-get 'e2wm:pst frame))
(defun e2wm:pst-set-instance (pst-instance)
  (e2wm:frame-param-set 'e2wm:pst pst-instance))

(defun e2wm:pst-get-prev-pst ()
  (e2wm:frame-param-get 'e2wm:prev-pst))
(defun e2wm:pst-set-prev-pst (pst-name)
  (e2wm:frame-param-set 'e2wm:prev-pst pst-name))

(defun e2wm:pst-copy-instance ()
  "[internal] Copy the current perspective instance."
  (let ((i (e2wm:pst-get-instance)))
    (make-e2wm:$pst
     :name   (e2wm:$pst-name   i)
     :wm     (wlf:copy-windows (e2wm:pst-get-wm))
     :type   (e2wm:$pst-type   i))))

(defun e2wm:pst-get-wm ()
  "Return the window layout object of the current perspective."
  (e2wm:aif (e2wm:pst-get-instance)
      (e2wm:$pst-wm it)))

(defun e2wm:pst-update-windows (&optional rebuild-windows)
  "Update all buffers of the windows and plug-ins.  If
REBUILD-WINDOWS is non-nil, windows are destroyed and new windows
are created."
  (e2wm:message "#PST-UPDATE")
  (e2wm:with-advice
   (let* ((instance (e2wm:pst-get-instance))
          (wm (e2wm:$pst-wm instance)))
     ;;(e2wm:debug-windows (e2wm:pst-get-wm))
     (unless rebuild-windows
       (wlf:wset-fix-windows wm))
     (when (or rebuild-windows 
               (not (wlf:wset-live-p wm)))
       (e2wm:message "  #PST-UPDATE > REBUILD")
       (wlf:refresh wm)
       (e2wm:aif (e2wm:$pst-main instance)
           (wlf:select wm it)))
     ;; Update task for the current perspective
     ;; (Plug-ins are updated by `e2wm:dp-base-update')
     (e2wm:pst-method-call e2wm:$pst-class-update instance wm)
     )) t)

(defun e2wm:pst-switch-to-buffer (buf)
  "[internal] Delegate the `switch' function of the current perspective."
  (e2wm:message "#PST-SWITCH %s" buf)
  (e2wm:pst-method-call e2wm:$pst-class-switch (e2wm:pst-get-instance) buf))

(defun e2wm:pst-pop-to-buffer (buf)
  "[internal] Delegate the `popup' function of the current perspective."
  (e2wm:message "#PST-POPUP %s" buf)
  (e2wm:pst-method-call e2wm:$pst-class-popup (e2wm:pst-get-instance) buf))

(defun e2wm:pst-change (next-pst-name)
  "Leave the current perspective and start the new perspective."
  (e2wm:message "#PST-CHANGE %s" next-pst-name)
  (let ((prev-pst-instance (e2wm:pst-get-instance))
        (next-pst-class (e2wm:pst-class-get next-pst-name))
        (e2wm:prev-selected-buffer (current-buffer))
        (prev-selected-buffer (current-buffer)))
    (when (e2wm:internal-buffer-p e2wm:prev-selected-buffer)
      (setq e2wm:prev-selected-buffer nil)
      (setq prev-selected-buffer nil))
    (cond
     ((null next-pst-class)
      (error "Perspective [%s] is not found." next-pst-name))
     (t
      (e2wm:aif prev-pst-instance
          (progn
            (e2wm:pst-method-call e2wm:$pst-class-leave it (e2wm:$pst-wm it))
            (unless (eql next-pst-name (e2wm:$pst-name it))
              (e2wm:pst-set-prev-pst (e2wm:$pst-name it)))))
      (let* ((next-pst-wm
              (e2wm:method-call 'e2wm:$pst-class-init 
                               next-pst-class
                               (format "[%s] init method is nil!" next-pst-name)))
             (next-pst-instance 
              (make-e2wm:$pst :name next-pst-name
                             :wm next-pst-wm
                             :type next-pst-class)))
        (e2wm:pst-set-instance next-pst-instance)
        (e2wm:pst-change-keymap (e2wm:$pst-keymap next-pst-instance))
        (e2wm:pst-method-call e2wm:$pst-class-start next-pst-instance 
                             (e2wm:$pst-wm next-pst-instance)))))
    (e2wm:pst-update-windows t)))

(defun e2wm:pst-change-prev ()
  "[internal] Change to the previous perspective."
  (e2wm:aif (e2wm:pst-get-prev-pst)
      (progn
        (e2wm:message "#PREV-PST : %s" it)
        (e2wm:pst-change it))))

(defvar e2wm:pst-minor-mode-keymap
      (e2wm:define-keymap
       '(("prefix Q"   . e2wm:stop-management)
         ("prefix l"   . e2wm:pst-update-windows-command)
         ("prefix n"   . e2wm:pst-history-down-command)
         ("prefix p"   . e2wm:pst-history-up-command)
         ("prefix <DEL>" . e2wm:pst-change-prev-pst-command)
         ) e2wm:prefix-key)
      "Common key map for all perspectives. (See `e2wm:pst-change-keymap')")

(defun e2wm:pst-change-keymap (new-keymap)
  "[internal] Add the perspective key map to the common key map."
  (let ((map (copy-keymap
              (or new-keymap e2wm:pst-minor-mode-keymap))))
    (when new-keymap
      (set-keymap-parent map e2wm:pst-minor-mode-keymap))
    (e2wm:aif (assq 'e2wm:pst-minor-mode minor-mode-map-alist)
        (setf (cdr it) map))))

(defun e2wm:pst-resume (pst-instance)
  "[internal] Resume the perspective which is suspended by the function `e2wm:pst-finish'."
  (e2wm:message "#PST-RESUME %s" pst-instance)
  ;; This function assumes that the window configuration is 
  ;; restored by `set-window-configuration'.
  (e2wm:pst-set-instance pst-instance)
  (e2wm:pst-change-keymap (e2wm:$pst-keymap pst-instance))
  (e2wm:pst-method-call e2wm:$pst-class-start pst-instance (e2wm:$pst-wm pst-instance)))

(defun e2wm:pst-finish ()
  "[internal] Suspend the current perspective for finishing e2wm or
switching the window configuration to the non-e2wm frame during
the other application calling `set-window-configuration'."
  (e2wm:message "#PST-FINISH")
  (let ((prev-pst-instance (e2wm:pst-get-instance)))
    (when prev-pst-instance
      (e2wm:pst-method-call e2wm:$pst-class-leave prev-pst-instance 
                           (e2wm:$pst-wm prev-pst-instance)))
    (e2wm:pst-set-instance nil)))

(defun e2wm:pst-window-option-get (wm window-name)
  "Return a plist of the plug-in option at the WINDOW-NAME window."
  (wlf:window-options 
   (wlf:get-winfo window-name (wlf:wset-winfo-list wm))))

(defun e2wm:pst-window-plugin-get (wm window-name)
  "Return a symbol of the plug-in at the WINDOW-NAME window. "
  (plist-get (e2wm:pst-window-option-get wm window-name)
             ':plugin))

(defun e2wm:pst-window-plugin-set (wm window-name plugin-name)
  "Set the plug-in at the WINDOW-NAME window."
  (plist-put (e2wm:pst-window-option-get wm window-name)
             ':plugin plugin-name))

(defun e2wm:pst-buffer-get (window-name)
  "Return the buffer object at the WINDOW-NAME window."
  (let ((wm (e2wm:pst-get-wm)))
    (when (wlf:window-name-p wm window-name)
      (wlf:get-buffer wm window-name))))

(defun e2wm:pst-buffer-set (window-name buffer &optional showp selectp)
  "Set the given BUFFER at the WINDOW-NAME window.
If SHOWP is non-nil, the hided window is displayed.
If SELECTP is non-nil, the window is selected."
  (let ((wm (e2wm:pst-get-wm)))
    (when (wlf:window-name-p wm window-name)
      (when (and showp (not (wlf:get-window wm window-name)))
        (wlf:show wm window-name))
      (wlf:set-buffer wm window-name buffer selectp)
      (when showp
        (set-window-point
         (wlf:get-window wm window-name) 
         (with-current-buffer buffer (point)))))))

(defun e2wm:pst-window-select (window-name)
  "Select the WINDOW-NAME window."
  (let ((wm (e2wm:pst-get-wm)))
    (when (wlf:window-name-p wm window-name)
      (wlf:select wm window-name))))

(defun e2wm:pst-window-select-main ()
  "Select the `main' window which is defined by the perspective.
If the perspective has no `main' window, this function does nothing."
  (let ((main (e2wm:$pst-main (e2wm:pst-get-instance)))
        (wm (e2wm:pst-get-wm)))
    (when (and main (wlf:window-name-p wm main))
      (wlf:select wm main))))

(defun e2wm:pst-window-toggle (window-name &optional selectp next-window)
  "Toggle visibility of the window specified by WINDOW-NAME.
If SELECTP is non-nil, it selects that window when opening it.
NEXT-WINDOW specifies the window to select when closing the
WINDOW-NAME window.  This function returns the name of the
selected window, or nil if none is selected."
  (let ((wm (e2wm:pst-get-wm)))
    (when (wlf:window-name-p wm window-name)
      (wlf:toggle wm window-name)
      (if (wlf:window-displayed-p wm window-name)
          (when selectp (wlf:select wm window-name) window-name)
        (when next-window (wlf:select wm next-window) next-window)))))

(defun e2wm:pst-show-history-main ()
  "Display the history top buffer at the `main' window which is
defined by the perspective."
  (e2wm:with-advice
   (let* ((instance (e2wm:pst-get-instance))
          (wm (e2wm:$pst-wm instance)))
     (e2wm:aif (e2wm:$pst-main instance)
         (wlf:set-buffer wm it (e2wm:history-get-main-buffer)))
     (e2wm:pst-update-windows))))

(defun e2wm:pst-after-save-hook ()
  "[internal] Hook for `after-save-hook'."
  (e2wm:message "$$ AFTER SAVE HOOK %S" this-command)
  ;; Ignore save events those are triggered by timers.
  (when this-command
    (e2wm:pst-method-call e2wm:$pst-class-save (e2wm:pst-get-instance))
    (e2wm:pst-update-windows)))

;;; Commands / Key bindings / Minor Mode
;;;--------------------------------------------------

(defun e2wm:pst-change-command ()
  (interactive)
  (let* ((pst-list (mapcar
                    (lambda (i)
                      (symbol-name (e2wm:$pst-class-name i))) 
                    e2wm:pst-list))
         (pst-name (completing-read "Chagne parspective: " pst-list)))
    (when pst-name
      (e2wm:pst-change (intern pst-name)))))
(defun e2wm:pst-window-select-main-command ()
  (interactive)
  (e2wm:pst-window-select-main))
(defun e2wm:pst-update-windows-command ()
  (interactive)
  (when (e2wm:managed-p)
    (e2wm:with-advice
     (let ((livep (wlf:wset-live-p (e2wm:pst-get-wm))))
       (wlf:reset-window-sizes (e2wm:pst-get-wm))
       (e2wm:pst-update-windows (not livep))))))
(defun e2wm:pst-change-prev-pst-command ()
  (interactive)
  (when (e2wm:managed-p)
    (e2wm:pst-change-prev)))
(defun e2wm:pst-history-forward-command ()
  (interactive)
  (when (e2wm:managed-p)
    (e2wm:history-forward)
    (e2wm:pst-show-history-main)))
(defun e2wm:pst-history-back-command ()
  (interactive)
  (when (e2wm:managed-p)
    (e2wm:history-back)
    (e2wm:pst-show-history-main)))
(defalias 'e2wm:pst-history-up-command 'e2wm:pst-history-forward-command)
(defalias 'e2wm:pst-history-down-command 'e2wm:pst-history-back-command)

(defvar e2wm:pst-minor-mode-setup-hook nil "This hook is called at end of setting up pst-minor-mode.")
(defvar e2wm:pst-minor-mode-abort-hook nil "This hook is called at end of aborting pst-minor-mode.")

(defvar e2wm:pst-minor-mode nil) ; dummy

;;グローバルでマイナーモードを定義
(define-minor-mode e2wm:pst-minor-mode
  "Perspective mode"
  :init-value nil
  :global t
  :lighter (:eval (if (e2wm:managed-p) 
                      (format " E2wm(%s)" (e2wm:$pst-name (e2wm:pst-get-instance)))
                    " E2wm(none)"))
  :keymap e2wm:pst-minor-mode-keymap
  :group 'e2wm:pst-mode
  (if e2wm:pst-minor-mode
      (progn
        (e2wm:pst-minor-mode-setup)
        (add-hook 'delete-frame-functions 'e2wm:delete-frame-functions)
        (run-hooks 'e2wm:pst-minor-mode-setup-hook))
    (e2wm:pst-minor-mode-abort)
    (remove-hook 'delete-frame-functions 'e2wm:delete-frame-functions)
    (run-hooks 'e2wm:pst-minor-mode-abort-hook)))

(defun e2wm:pst-minor-mode-setup ()
  (add-to-list 'after-make-frame-functions 'e2wm:override-after-make-frame)
  (ad-activate-regexp "^e2wm:ad-frame-override$" t)
  (e2wm:pst-minor-mode-enable-frame)
)

(defun e2wm:pst-minor-mode-abort ()
  (setq after-make-frame-functions
        (remove 'e2wm:override-after-make-frame after-make-frame-functions))
  (ad-deactivate-regexp "^e2wm:ad-frame-override$")
  (e2wm:pst-minor-mode-disable-frame)
)

;;管理対象frameだけキーマップを有効にするアドバイス

(defvar e2wm:pst-minor-mode-keymap-backup nil "[internal]")
(defvar e2wm:pst-minor-mode-keymap-blank (make-sparse-keymap) "[internal]")

(defun e2wm:pst-minor-mode-disable-frame ()
  (e2wm:message "## PST MM DISABLED")
  ;;グローバルマイナーモードは有効のままで、アドバイス、キーマップのみ無効にする
  ;;特定のフレームで有効というイメージ
  (setq e2wm:pst-minor-mode-keymap-backup e2wm:pst-minor-mode-keymap)
  (e2wm:aif (assq 'e2wm:pst-minor-mode minor-mode-map-alist)
      (setf (cdr it) e2wm:pst-minor-mode-keymap-blank))

  (remove-hook 'kill-buffer-hook 'e2wm:kill-buffer-hook)
  (remove-hook 'window-configuration-change-hook 
               'e2wm:override-window-cfg-change)
  (remove-hook 'completion-setup-hook 'e2wm:override-setup-completion)
  (remove-hook 'after-save-hook 'e2wm:pst-after-save-hook)
  (remove-hook 'next-error-hook 'e2wm:select-window-point)
  (setq display-buffer-function nil)
  (ad-deactivate-regexp "^e2wm:ad-override$")
  )

(defun e2wm:pst-minor-mode-enable-frame ()
  (e2wm:message "## PST MM ENABLED")
  (e2wm:aif (assq 'e2wm:pst-minor-mode minor-mode-map-alist)
      (setf (cdr it) e2wm:pst-minor-mode-keymap-backup))
  (setq e2wm:pst-minor-mode-keymap-backup nil)

  (ad-activate-regexp "^e2wm:ad-override" t)
  (add-hook 'kill-buffer-hook 'e2wm:kill-buffer-hook)
  (add-hook 'window-configuration-change-hook
            'e2wm:override-window-cfg-change)
  (add-hook 'completion-setup-hook 'e2wm:override-setup-completion)
  (add-hook 'after-save-hook 'e2wm:pst-after-save-hook)
  (add-hook 'next-error-hook 'e2wm:select-window-point)
  (setq display-buffer-function 'e2wm:override-special-display-function))

(defun e2wm:pst-minor-mode-switch-frame (frame)
  (e2wm:message "## PST MM SWITCH [%s] / %s" (e2wm:managed-p frame) frame)
  (cond
   ((e2wm:managed-p frame)
    (e2wm:pst-minor-mode-enable-frame))
   (t
    (e2wm:pst-minor-mode-disable-frame))))

(defadvice handle-switch-frame (around e2wm:ad-frame-override (event))
  ad-do-it
  (e2wm:message "## FRAME SWITCH [%s] <- (%s)" event (selected-frame))
  (when (eq 'switch-frame (car event))
    (e2wm:pst-minor-mode-switch-frame (cadr event))))

(defun e2wm:override-after-make-frame (frame)
  (e2wm:message "## MAKE FRAME [%s] <- (%s)" frame (selected-frame))
  (e2wm:pst-minor-mode-switch-frame frame))

(defadvice handle-delete-frame (around e2wm:ad-frame-override (event))
  (e2wm:message "## 1 FRAME DELETE [%s] " event)
  (let* ((frame (car (cadr event)))
         (next-frame (next-frame frame)))
    (e2wm:message "## 2 FRAME DELETE [%s] -> (%s)" frame next-frame)
    (e2wm:pst-minor-mode-switch-frame next-frame)
    (select-frame next-frame))
  ad-do-it)

(defun e2wm:delete-frame-functions (frame)
  (e2wm:message "## DELETE FRAME HOOK [%s] " frame)
  (let* ((next-frame (car (filtered-frame-list #'e2wm:managed-p))))
    (when next-frame
      (e2wm:message "## NEXT FRAME [%s] -> (%s)" frame next-frame)
      (e2wm:pst-minor-mode-switch-frame next-frame)
      (select-frame next-frame))))

(defadvice other-frame (after e2wm:ad-frame-override)
  (e2wm:message "## OTHER FRAME [%s] " (selected-frame))
  (e2wm:pst-minor-mode-switch-frame (selected-frame)))

;;; Perspective Set
;; 好みのパースペクティブのセットを作って選べるようにする

(defun e2wm:pstset-defaults()
  ;;abstract classとarray以外を全部つっこむ
  (e2wm:pstset-define
   (nreverse
    (loop for i in e2wm:pst-list
          unless (or (memq (e2wm:$pst-class-name i) '(array))
                     (e2wm:pst-class-abstract-p i))
          collect (e2wm:$pst-class-name i)))))

(defun e2wm:pstset-define (names)
  ;;引数：パースペクティブのシンボルのリスト
  (loop for i in names
        unless (e2wm:pst-class-get i)
        do (error "Perspective [%s] not found." i))
  (e2wm:frame-param-set 'e2wm:pstset names))

(defun e2wm:pstset-get-current-pstset ()
  (e2wm:frame-param-get 'e2wm:pstset))

(defun e2wm:pstset-next-pst-command ()
  ;; 現在のセットで次のパースペクティブに切り替える
  (interactive)
  (e2wm:aif (e2wm:pst-get-instance)
      (let* ((pset (e2wm:pstset-get-current-pstset))
             (now (e2wm:$pst-name it))
             (pos (position now pset)))
        (e2wm:aand pos (nth (1+ it) pset)
                  (e2wm:pst-change it)))))

(defun e2wm:pstset-prev-pst-command ()
  ;; 現在のセットで前のパースペクティブに切り替える
  (interactive)
  (e2wm:aif (e2wm:pst-get-instance)
      (let* ((pset (e2wm:pstset-get-current-pstset))
             (now (e2wm:$pst-name it))
             (pos (position now pset)))
        (e2wm:aand pos (nth (1- it) pset)
                  (e2wm:pst-change it)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Advices / Overriding Functions

(defadvice switch-to-buffer (around
                             e2wm:ad-override
                             (buf &optional norecord force-same-window))
  (e2wm:message "#SWITCH-TO-BUFFER %s" buf)
  (let (overrided)
    (when (and buf 
               (not e2wm:ad-now-overriding) ; 再入してなくて
               (e2wm:managed-p)) ; 管理対象フレームの場合は乗っ取る
      (e2wm:with-advice
       (e2wm:message "#AD-SWITCH-TO-BUFFER %s" buf)
       (e2wm:history-add buf)
       (setq overrided (e2wm:pst-switch-to-buffer (get-buffer-create buf)))))
    (if overrided
        (progn
          (set-buffer buf)
          (setq ad-return-value (get-buffer-create buf)))
      ad-do-it))) ; それ以外はもとの関数へ（画面更新はしないので必要な場合は自分でする）

(defadvice pop-to-buffer (around 
                          e2wm:ad-override
                          (buf &optional other-window norecord))
  (e2wm:message "#POP-TO-BUFFER %s" buf)
  (let (overrided)
    (when (and buf 
               (not e2wm:ad-now-overriding) ; 再入してなくて
               (e2wm:managed-p)) ; 管理対象フレームの場合は乗っ取る
      (e2wm:with-advice
       (e2wm:message "#AD-POP-TO-BUFFER %s" buf)
       (e2wm:history-add buf)
       (setq overrided (e2wm:pst-pop-to-buffer (get-buffer-create buf)))))
    (if overrided
        (progn 
          (set-buffer buf)
          (setq ad-return-value (get-buffer-create buf)))
      ad-do-it))) ; それ以外はもとの関数へ（画面更新はしないので必要な場合は自分でする）

(defun e2wm:after-bury-buffer (buried-buffer window)
  "[internal] This function is called after `bury-buffer' or
`quit-window' call, resets the buffer tracked by e2wm and
removes the buried buffer from the history list."
  ;; manage the current buffer in e2wm
  (when (e2wm:managed-p)
    (let ((win-name (wlf:get-window-name (e2wm:pst-get-wm) window)))
      (when win-name
        (e2wm:with-advice
         (e2wm:pst-buffer-set win-name (window-buffer window))))
      ;; remove the buffer from the history
      (when (get-buffer buried-buffer)
        (e2wm:message "#REMOVED BUFFER %s" buried-buffer)
        (e2wm:history-delete buried-buffer))
      ;; execute plugins -- only for history plugin.
      (when this-command
        (e2wm:pst-update-windows)))))

(defadvice quit-window (around
                        e2wm:ad-override
                        (&optional kill window))
  "[internal] call `e2wm:after-bury-buffer'."
  (cond
   ((e2wm:managed-p)
    (e2wm:message "#QUIT-WINDOW %s %s" kill window)
    (let ((curwin (or window (selected-window)))
          (buffer (window-buffer window)))
      ad-do-it
      (e2wm:after-bury-buffer buffer curwin)))
   (t ad-do-it)))

(eval-and-compile
  (unless (fboundp 'window-normalize-buffer)
    (defun window-normalize-buffer (buffer-or-name)
      "Return buffer specified by BUFFER-OR-NAME.
 (This function is copied from Emacs 24 for the fallback on Emacs 23.)"
      (cond
       ((not buffer-or-name)
        (current-buffer))
       ((bufferp buffer-or-name)
        (if (buffer-live-p buffer-or-name)
            buffer-or-name
          (error "Buffer %s is not a live buffer" buffer-or-name)))
       ((get-buffer buffer-or-name))
       (t
        (error "No such buffer %s" buffer-or-name))))))

(defadvice bury-buffer (around
                        e2wm:ad-override
                        (&optional buffer-or-name))
  "[internal] call `e2wm:after-bury-buffer'."
  (cond
   ((e2wm:managed-p)
    (e2wm:message "#BURY-BUFFER %s" buffer-or-name)
    (let ((curwin (selected-window))
          (buffer (window-normalize-buffer buffer-or-name)))
      ad-do-it
      (e2wm:after-bury-buffer buffer curwin)))
   (t ad-do-it)))


(defun e2wm:override-special-display-function (buf &optional args)
  (e2wm:message "#SPECIAL-DISPLAY-FUNC %s / %S - %S" buf (not e2wm:ad-now-overriding) (e2wm:managed-p))
  (let (overrided)
    (when (and 
           buf 
           (not e2wm:ad-now-overriding) ; 再入してなくて
           (e2wm:managed-p)) ; 管理対象フレームの場合は乗っ取る
      (e2wm:with-advice
       (e2wm:message "#AD-SPECIAL-DISPLAY-FUNC %s" buf)
       (e2wm:history-add buf)
       (save-excursion
         (setq overrided (e2wm:pst-pop-to-buffer (get-buffer-create buf))))))
    (if overrided
        (progn 
          ;(set-buffer buf)
          (get-buffer-window buf)) ;return value
      (cond
       ((e2wm:managed-p)
        (e2wm:message "#DISPLAY-BUFFER / managed frame") ;;適当な場所に表示する
        (set-window-buffer (selected-window) buf)
        ;(set-buffer buf) 
        (selected-window)) ;return value
       (t
        (e2wm:message "#DISPLAY-BUFFER / Non managed frame ") ;;適当な場所に表示する
        (e2wm:with-advice
         (let (special-display-function)
           (display-buffer buf))))) ;return value
      )))

(defun e2wm:kill-buffer-hook ()
  "[internal] Update windows which showed the killed buffer.
Called via `kill-buffer-hook'."
  (e2wm:message "#KILL HOOK")
  (when (and (e2wm:history-recordable-p (current-buffer))
             (e2wm:managed-p))
    ;; If kill is *not* called by command, don't change windows
    (when this-command
      ;; search through the existing windows which show the killed buffer
      (let* ((killedbuf (current-buffer))
             (wm (e2wm:pst-get-wm))
             (wins (loop for winfo in (wlf:wset-winfo-list wm)
                         for wname = (wlf:window-name winfo)
                         when (equal (wlf:get-buffer wm wname) killedbuf)
                         collect wname))
             (buffers (e2wm:history-get-nearest killedbuf (length wins)))
             (main-wname (e2wm:$pst-main (e2wm:pst-get-instance))))
        (cond
         (buffers
          (loop for wname in wins
                for buf in buffers
                do 
                (when (equal wname main-wname)
                  (e2wm:history-add buf))
                (wlf:set-buffer wm wname buf)))
         (main-wname 
          (wlf:set-buffer wm main-wname e2wm:c-blank-buffer)))))
    ;; remove it from the history list
    (e2wm:history-delete (current-buffer))
    (when this-command
      (e2wm:pst-update-windows))))

;; delete-other-windows対策

(defvar e2wm:delete-other-windows-permission nil "[internal] If this
value is t, one can execute `delete-other-windows' under the e2wm
management. For window-layout.el.")

(defadvice wlf:clear-windows (around e2wm:ad-override)
  (let ((e2wm:delete-other-windows-permission t))
    ad-do-it))

(defadvice delete-other-windows (around e2wm:ad-override)
  (when (or e2wm:delete-other-windows-permission (not (e2wm:managed-p)))
    ad-do-it))

;; コンパイルエラーのような他のウインドウへの表示をする拡張への対応
;; compile-goto-error の仕組みを使うものについては next-error-hook で実行

(defun e2wm:move-window-point (&optional buf)
  ;; ウインドウの表示位置のみを更新
  (let ((buf (or buf (current-buffer))))
    (when (and
           (e2wm:managed-p)
           (eq (wlf:get-window (e2wm:pst-get-wm) 'sub) (selected-window))
           (not (eql (selected-window) (get-buffer-window buf))))
      (set-window-point 
       (get-buffer-window buf)
       (with-current-buffer buf (point))))))

(defun e2wm:select-window-point (&optional buf)
  ;; ウインドウを選択して表示位置を更新
  (let ((buf (or buf (current-buffer))))
    (when (and
           (e2wm:managed-p)
           (eq (wlf:get-window (e2wm:pst-get-wm) 'sub) (selected-window))
           (not (eql (selected-window) (get-buffer-window buf))))
      (set-window-point 
       (get-buffer-window buf)
       (with-current-buffer buf (point)))
      (select-window (get-buffer-window buf)))))

;; set-window-configuration 対策
;; いろいろ試行錯誤中。

(defvar e2wm:override-window-ext-managed nil) ; Elscreenのように、別アプリがフレームを管理している場合はt

(defun e2wm:debug-windows (wm)
  (e2wm:message " # WINDOWS : %s" 
               (loop for winfo in (wlf:wset-winfo-list wm)
                     collect (wlf:window-window winfo))))

;;e2wm:$wcfg ウインドウ配置構造体
;; wcfg  : 本来のcurrent-window-configurationでとれるウインドウ配置オブジェクト
;; pst   : パースペクティブのインスタンスのコピー
;; count : デバッグ用カウンタ
(defstruct e2wm:$wcfg wcfg pst count)

(defun e2wm:override-custom-wcfg-p (cfg)
  (e2wm:$wcfg-p cfg))

(defvar e2wm:override-window-cfg-change-now nil) ; e2wm:override-window-cfg-change 実行中ならt。再帰呼び出しを防ぐ。
(defvar e2wm:override-window-cfg-backup nil "[internal] Backup window configuration.")

(defun e2wm:override-window-cfg-change ()
  ;; window-configuration-change-hook関数
  (when (and (e2wm:managed-p) ; e2wm管理中で
             (null e2wm:override-window-cfg-change-now) ; 初回実行で
             (= (minibuffer-depth) 0) ; ミニバッファ実行中でなくて
             (and e2wm:override-window-cfg-backup ; 補完前のウインドウ配置が空でなくて
                  (not (compare-window-configurations ; 配置が違ってたら
                        e2wm:override-window-cfg-backup 
                        (current-window-configuration)))))
    (setq e2wm:override-window-cfg-change-now t)
    (unwind-protect
        (e2wm:override-restore-window-cfg) ; 配置を戻す
      (setq e2wm:override-window-cfg-change-now nil))))

(defun e2wm:override-setup-completion ()
  ;;completionバッファが終了したとき、set-window-configurationが呼ばれずに
  ;;window配置が元に戻される。なので、completionから戻ったときには
  ;;windwo-configuration-change-hookを捕まえて自前で
  ;;window配置を直すようにする。
  (when (and (e2wm:managed-p) (null e2wm:override-window-cfg-backup))
    (e2wm:message "#OVERRIDE-SETUP-COMPLETION")
    ;;(e2wm:debug-windows (e2wm:pst-get-wm))
    (setq e2wm:override-window-cfg-backup 
          (current-window-configuration))))

(defun e2wm:override-restore-window-cfg ()
  (interactive)
  (when e2wm:override-window-cfg-backup
    (e2wm:message "#RESTORE-WINDOW-CFG")
    (set-window-configuration e2wm:override-window-cfg-backup)
    (setq e2wm:override-window-cfg-backup nil)
    (let ((i (e2wm:pst-get-instance)))
      (e2wm:aif (e2wm:$pst-main i)
        (wlf:select (e2wm:$pst-wm i) it)))))

(defvar e2wm:override-window-cfg-count 0 "[internal] Window configuration counter")

(defadvice current-window-configuration (around e2wm:ad-override)
  (let ((cfg ad-do-it))
    (incf e2wm:override-window-cfg-count)
    (e2wm:message "#CURRENT-WINDOW-CONFIGURATION %s" 
                 e2wm:override-window-cfg-count)
    (if (e2wm:managed-p)
        (let ((data (e2wm:pst-copy-instance)))
          (setq ad-return-value
                (make-e2wm:$wcfg :wcfg cfg :pst data 
                      :count e2wm:override-window-cfg-count))))))

(defadvice window-configuration-p (around e2wm:ad-override-long (cfg))
  (setq ad-return-value (or (e2wm:override-custom-wcfg-p cfg) ad-do-it)))

(defadvice window-configuration-frame (around e2wm:ad-override-long (cfg))
  (when (e2wm:override-custom-wcfg-p cfg)
    (ad-set-arg 0 (e2wm:$wcfg-wcfg cfg)))
  ad-do-it)

(defadvice compare-window-configurations (around e2wm:ad-override-long (cfg1 cfg2))
  (when (e2wm:override-custom-wcfg-p cfg1)
    (ad-set-arg 0 (e2wm:$wcfg-wcfg cfg1)))
  (when (e2wm:override-custom-wcfg-p cfg2)
    (ad-set-arg 1 (e2wm:$wcfg-wcfg cfg2)))
  ad-do-it
  (when (and ad-return-value (e2wm:managed-p))
    (e2wm:message "#COMPARE-WINDOW-CONFIGURATIONS = %s" ad-return-value)
    ;;(e2wm:debug-windows (e2wm:pst-get-wm))
    ))

(defadvice set-window-configuration (around e2wm:ad-override-long (cfg))
  (e2wm:message "#SET-WINDOW-CONFIGURATION -->")
  (cond 
   ((e2wm:override-custom-wcfg-p cfg)
    ;;管理対象であればwindowオブジェクトを元に戻す
    (let ((pst-instance (e2wm:$wcfg-pst cfg))
          (count (e2wm:$wcfg-count cfg)))
      (ad-set-arg 0 (e2wm:$wcfg-wcfg cfg))
      (e2wm:message "#SET-WINDOW-CONFIGURATION (ad-do-it)")
      ad-do-it
      ;;(e2wm:debug-windows (e2wm:$pst-wm pst-instance))
      (when e2wm:pst-minor-mode
        (cond
         ((e2wm:managed-p)
          ;;(e2wm:message "#AD-SET-WINDOW-CONFIGURATION SET %s" pst-instance)
          (e2wm:message "#AD-SET-WINDOW-CONFIGURATION SET %s" count)
          (e2wm:pst-set-instance pst-instance))
         (t
          (e2wm:message "#AD-SET-WINDOW-CONFIGURATION RESUME %s" pst-instance)
          (e2wm:pst-set-instance pst-instance)
          (e2wm:pst-resume pst-instance))))))
   (t
    ;;管理してない配置の場合はパースペクティブを無効にする
    (when (and (e2wm:managed-p) (null e2wm:override-window-ext-managed))
      (e2wm:message "#AD-SET-WINDOW-CONFIGURATION FINISH")
      (e2wm:pst-finish)
      (e2wm:pst-set-instance nil))
    ad-do-it))
  (e2wm:message "#SET-WINDOW-CONFIGURATION <-- %s" ad-return-value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Plugin Framework

;; e2wm:$plugin構造体
;; name   : プラグインの symbol 
;; title  : 人が読む用のプラグインの名前
;; update : プラグイン本体の関数
(defstruct e2wm:$plugin name title update)

(defvar e2wm:plugin-list nil "[internal] Plugin registory.")
(setq e2wm:plugin-list nil)

(defun e2wm:plugin-register (name title update-function)
  ;;プラグインの登録
  (e2wm:plugin-delete name)
  (push (make-e2wm:$plugin
         :name name
         :title title
         :update update-function)
        e2wm:plugin-list))

(defun e2wm:plugin-delete (name)
  ;;プラグインの登録削除
  (e2wm:delete! name e2wm:$plugin-name e2wm:plugin-list))

(defun e2wm:plugin-get (name)
  ;;プラグイン構造体を名前から取ってくる
  (if name 
      (e2wm:aif (e2wm:find name 'e2wm:$plugin-name e2wm:plugin-list)
          it (e2wm:message "Plugin not found [%s]." name) nil)))

(defun e2wm:plugin-exec-update (frame wm)
  ;;各windowのプラグインを実行
  (loop for winfo in (wlf:wset-winfo-list wm)
        for plugin-name = (wlf:window-option-get winfo :plugin)
        for plugin = (e2wm:plugin-get plugin-name)
        if (and (wlf:window-live-window winfo) plugin)
        do 
        (condition-case err
            (funcall (e2wm:$plugin-update plugin) frame wm winfo)
          (nil (e2wm:message "Plugin Error %s [%s]" plugin-name err)))))

(defun e2wm:plugin-exec-update-by-plugin-name (frame wm exec-plugin-name)
  ;;指定のプラグインだけを実行（プラグインから更新のために呼ばれる）
  (loop for winfo in (wlf:wset-winfo-list wm)
        for plugin-name = (wlf:window-option-get winfo :plugin)
        for plugin = (e2wm:plugin-get plugin-name)
        if (and (wlf:window-live-window winfo) plugin
                (eq exec-plugin-name plugin-name))
        do 
        (condition-case err
            (funcall (e2wm:$plugin-update plugin) frame wm winfo)
          (nil (e2wm:message "Plugin Error %s [%s]" plugin-name err)))))

(defun e2wm:plugin-switch (plugin-name)
  ;;現在選択されているウインドウのプラグインを取り替える
  (let* ((wm (e2wm:pst-get-wm))
         (wname (wlf:get-window-name wm (selected-window)))
         (plugin-symbol (if (symbolp plugin-name) 
                            plugin-name
                          (intern plugin-name))))
    (when wname
      (e2wm:pst-window-plugin-set wm wname plugin-symbol)
      (e2wm:pst-update-windows))))

(defun e2wm:plugin-switch-command ()
  (interactive)
  ;;プラグインを選択して取り替える
  (let* ((wm (e2wm:pst-get-wm))
         (wname (wlf:get-window-name wm (selected-window)))
         (cplg (or (if wname 
                       (e2wm:pst-window-plugin-get wm wname)) 
                   "No plugin"))
         (plg-list (mapcar 
                    (lambda (i) (symbol-name (e2wm:$plugin-name i)))
                    e2wm:plugin-list))
         (completion-ignore-case t)
         (plg-name (completing-read
                    (format "Chagne plugin [current: %s] -> : " cplg)
                    plg-list)))
    (when plg-name
      (e2wm:plugin-switch plg-name))))

(defun e2wm:plugin-remove-command ()
  (interactive)
  ;;現在選択されているウインドウのプラグインを外す
  (let* ((wm (e2wm:pst-get-wm))
         (wname (wlf:get-window-name wm (selected-window))))
    (when wname
      (e2wm:pst-window-plugin-set wm wname nil)
      (e2wm:pst-update-windows))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Menu Definition

(defvar e2wm:pst-minor-mode-menu-spec nil)

(defun e2wm:menu-pst-selected-p (name)
  ;;現在のパースペクティブがnameかどうか
  (eq name (e2wm:$pst-name (e2wm:pst-get-instance))))

(defun e2wm:menu-plugin-selected-p (name)
  ;;現在のウインドウのプラグインがnameかどうか
  (let* ((wm (e2wm:pst-get-wm))
         (wname (wlf:get-window-name wm (selected-window))))
    (when wname
      (eq name (e2wm:pst-window-plugin-get wm wname)))))

(defun e2wm:menu-plugin-working-p ()
  ;;現在のウインドウにプラグインがあるかどうか
  (let* ((wm (e2wm:pst-get-wm))
         (wname (wlf:get-window-name wm (selected-window))))
    (e2wm:pst-window-plugin-get wm wname)))

(defun e2wm:menu-define ()
  (let (perspectives plugins)
    (setq perspectives
          (loop for i in e2wm:pst-list
                for n = (e2wm:$pst-class-name i)
                collect 
                (vector (e2wm:$pst-class-title i) 
                        `(lambda () (interactive) (e2wm:pst-change ',n)) 
                        :selected `(e2wm:menu-pst-selected-p ',n)
                        :style 'toggle)))
    (setq plugins
          (loop for i in e2wm:plugin-list
                for n = (e2wm:$plugin-name i)
                collect 
                (vector (e2wm:$plugin-title i)
                        `(lambda () (interactive) (e2wm:plugin-switch ',n))
                        :selected `(e2wm:menu-plugin-selected-p ',n)
                        :style 'toggle)))

    (setq e2wm:pst-minor-mode-menu-spec 
          `("E2wm"
            ["History Forward" e2wm:pst-history-forward-command t]
            ["History Back"    e2wm:pst-history-back-command t]
            ["Update Windows"  e2wm:pst-update-windows-command t]
            "----"
            ["Quit E2wm"  e2wm:stop-management t]
            "----"
            "Perspectives" ,@(nreverse perspectives)
            "----"
            "Plugins" ,@(nreverse plugins)
            ["Remove Current Plugin" e2wm:plugin-remove-command
             (e2wm:menu-plugin-working-p)]
            ))
    (easy-menu-define e2wm-menu-map
      e2wm:pst-minor-mode-keymap "E2wm menu map" 
      e2wm:pst-minor-mode-menu-spec)
    ;; (easy-menu-add e2wm-menu-map e2wm:pst-minor-mode-keymap)
    ))

;; (e2wm:menu-define)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Plugin Definition

(defface e2wm:face-history-list-normal
  '((t :foreground "DarkSlateBlue"))
  "Face for e2wm history list." :group 'e2wm)
(defface e2wm:face-history-list-select1
  '((t :foreground "OrangeRed" :background "Lightsteelblue1"))
  "Face for e2wm history list." :group 'e2wm)
(defface e2wm:face-history-list-select2
  '((t :foreground "Blue" :background "WhiteSmoke"))
  "Face for e2wm history list." :group 'e2wm)

;;; history-list / バッファ・履歴一覧
;;;--------------------------------------------------

(defun e2wm:def-plugin-history-list (frame wm winfo)
  (let ((wname (wlf:window-name winfo))
        (win (wlf:window-live-window winfo))
        (buf (get-buffer " *WM:History*")) current-pos)
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create " *WM:History*"))
      (with-current-buffer buf
        (e2wm:def-plugin-history-list-mode)
        (setq buffer-read-only t)
        (setq truncate-lines t)
        (buffer-disable-undo buf)
        (hl-line-mode 1)))
    (with-current-buffer buf
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (erase-buffer)
            (goto-char (point-min))
            (let ((history (e2wm:history-get))
                  (history-backup (reverse (e2wm:history-get-backup)))
                  (cnt 1))
              (loop for h in (append history-backup history)
                    with main-buf = (e2wm:history-get-main-buffer)
                    for name = (if (stringp h) h (buffer-name h))
                    do (insert 
                        (e2wm:tp 
                         (e2wm:rt 
                          (format
                           "%3s %s %s\n" cnt name 
                           (if (buffer-modified-p h) "*" ""))
                          (if (eql h main-buf) 'e2wm:face-history-list-select1
                            'e2wm:face-history-list-normal))
                         'e2wm:buffer h))
                    (incf cnt))
              (goto-line (1+ (length history-backup)))
              (setq current-pos (point))
              (setq mode-line-format 
                    '("-" mode-line-mule-info
                      " " mode-line-position "-%-"))
              (setq header-line-format
                    (format "Buffer History [%i]" (1- cnt)))))
        (setq buffer-read-only t)))
    (wlf:set-buffer wm wname buf)
    (when win (set-window-point win current-pos))))

(defvar e2wm:def-plugin-history-list-mode-map 
  (e2wm:define-keymap 
   '(("k" . previous-line)
     ("j" . next-line)
     ("d" . e2wm:def-plugin-history-list-kill-command)
     ("<SPC>" . e2wm:def-plugin-history-list-show-command)
     ("C-m"   . e2wm:def-plugin-history-list-select-command)
     ("q"     . e2wm:pst-window-select-main-command)
     )))

(define-derived-mode e2wm:def-plugin-history-list-mode fundamental-mode "History")

(defun e2wm:def-plugin-history-list-kill-command ()
  (interactive)
  (when (e2wm:managed-p)
    (let (buf)
      (save-excursion
        (beginning-of-line)
        (setq buf (get-text-property (point) 'e2wm:buffer)))
      (when (and buf (buffer-live-p buf))
        (kill-buffer buf)))))

(defun e2wm:def-plugin-history-list-forward-command ()
  (interactive)
  (when (e2wm:managed-p)
    (e2wm:pst-history-forward-command)))

(defun e2wm:def-plugin-history-list-back-command ()
  (interactive)
  (when (e2wm:managed-p)
    (e2wm:pst-history-back-command)))

(defun e2wm:def-plugin-history-list-select-command ()
  (interactive)
  (when (e2wm:managed-p)
    (e2wm:def-plugin-history-list-show-command)
    (e2wm:pst-window-select-main)))

(defun e2wm:def-plugin-history-list-show-command ()
  (interactive)
  (when (e2wm:managed-p)
    (let (buf)
      (save-excursion
        (beginning-of-line)
        (setq buf (get-text-property (point) 'e2wm:buffer)))
      (when (and buf (buffer-live-p buf))
        (e2wm:history-add buf)
        (e2wm:pst-show-history-main)))))

(e2wm:plugin-register 'history-list 
                     "History List"
                     'e2wm:def-plugin-history-list)

;;; history-list2 / バッファ・履歴一覧 (two専用)
;;;--------------------------------------------------

(defun e2wm:def-plugin-history-list2-get-plugin-buffer (wm)
  ;; pluginがprev-mainかhistory-nthのwindowのバッファを取ってくる
  (loop for winfo in (wlf:wset-winfo-list wm)
        for plugin-name = (wlf:window-option-get winfo :plugin)
        if (and (wlf:window-live-window winfo) plugin-name
                (memq plugin-name '(history-nth main-prev)))
        return (wlf:get-buffer wm (wlf:window-name winfo))))

(defun e2wm:def-plugin-history-list2 (frame wm winfo)
  (let ((wname (wlf:window-name winfo))
        (win (wlf:window-live-window winfo))
        (buf (get-buffer " *WM:History2*")) current-pos)
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create " *WM:History2*"))
      (with-current-buffer buf
        (e2wm:def-plugin-history-list2-mode)
        (setq buffer-read-only t)
        (setq truncate-lines t)
        (buffer-disable-undo buf)
        (hl-line-mode 1)))
    (with-current-buffer buf
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (setq current-pos (point))
            (erase-buffer)
            (goto-char (point-min))
            (let* ((history (e2wm:history-get))
                   (history-backup (reverse (e2wm:history-get-backup)))
                   (main-buf   (wlf:get-buffer wm 'left))
                   (second-buf (wlf:get-buffer wm 'right))
                   (cnt 1))
              (loop for h in (append history-backup history)
                    for name = (if (stringp h) h (buffer-name h))
                    do (insert 
                        (e2wm:tp 
                         (e2wm:rt 
                          (format
                           "%2s%2s %2s %s %s\n" 
                           (if (eql h main-buf) "<-" "")
                           (if (eql h second-buf) "->" "")
                           cnt name 
                           (if (buffer-modified-p h) "*" ""))
                          (cond
                           ((eql h main-buf) 'e2wm:face-history-list-select1)
                           ((eql h second-buf) 'e2wm:face-history-list-select2)
                           (t
                            'e2wm:face-history-list-normal)))
                         'e2wm:buffer h))
                    (incf cnt))
              (goto-char current-pos)
              (setq mode-line-format 
                    '("-" mode-line-mule-info
                      " " mode-line-position "-%-"))
              (setq header-line-format
                    (format "Buffer History [%i]" (1- cnt)))))
        (setq buffer-read-only t)))
    (wlf:set-buffer wm wname buf)
    (when win (set-window-point win current-pos))))

(defvar e2wm:def-plugin-history-list2-mode-map 
  (e2wm:define-keymap 
   '(
     ("p" . previous-line)
     ("n" . next-line)
     ("k" . previous-line)
     ("j" . next-line)

     ("h"       . e2wm:def-plugin-history-list2-show-left-command)
     ("b"       . e2wm:def-plugin-history-list2-show-left-command)
     ("C-b"     . e2wm:def-plugin-history-list2-show-left-command)
     ("<left>"  . e2wm:def-plugin-history-list2-show-left-command)
     ("<SPC>"   . e2wm:def-plugin-history-list2-show-left-command)

     ("l"       . e2wm:def-plugin-history-list2-show-right-command)
     ("f"       . e2wm:def-plugin-history-list2-show-right-command)
     ("C-f"     . e2wm:def-plugin-history-list2-show-right-command)
     ("<right>" . e2wm:def-plugin-history-list2-show-right-command)

     ("d"       . e2wm:def-plugin-history-list-kill-command)
     ("C-m"     . e2wm:def-plugin-history-list2-select-command)
     ("q"       . e2wm:pst-window-select-main-command)
     )))

(define-derived-mode e2wm:def-plugin-history-list2-mode fundamental-mode "History")

(defun e2wm:def-plugin-history-list2-show-right-command ()
  (interactive)
  (when (e2wm:managed-p)
    (let (buf)
      (save-excursion
        (beginning-of-line)
        (setq buf (get-text-property (point) 'e2wm:buffer)))
      (when (and buf (buffer-live-p buf))
        (e2wm:pst-buffer-set 'right buf)
        (e2wm:plugin-exec-update-by-plugin-name
         (selected-frame) (e2wm:pst-get-wm) 'history-list2)))))

(defun e2wm:def-plugin-history-list2-show-left-command ()
  (interactive)
  (when (e2wm:managed-p)
    (let (buf)
      (save-excursion
        (beginning-of-line)
        (setq buf (get-text-property (point) 'e2wm:buffer)))
      (when (and buf (buffer-live-p buf))
        (cond
         ((eql buf (e2wm:pst-buffer-get 'left))
          (e2wm:pst-buffer-set 'right buf))
         (t
          (e2wm:pst-buffer-set 'left buf)))
        (e2wm:plugin-exec-update-by-plugin-name
         (selected-frame) (e2wm:pst-get-wm) 'history-list2)))))

(defun e2wm:def-plugin-history-list2-select-command ()
  (interactive)
  (when (e2wm:managed-p)
    (e2wm:def-plugin-history-list2-show-left-command)
    (e2wm:pst-window-select-main)))

(e2wm:plugin-register 'history-list2 
                     "History List (two)"
                     'e2wm:def-plugin-history-list2)

;;; dired / メインバッファの位置のファイル一覧
;;;--------------------------------------------------

;; 一番簡単なプラグイン
;; diredに丸投げ

(defun e2wm:def-plugin-dired (frame wm winfo) 
  (let* ((buf (e2wm:history-get-main-buffer))
         (dir (with-current-buffer buf 
                (or default-directory ".")))
         (dbuf (dired-noselect dir)))
    (with-current-buffer dbuf (revert-buffer))
    (wlf:set-buffer wm (wlf:window-name winfo) dbuf)))

(e2wm:plugin-register 'dired
                     "Dired"
                     'e2wm:def-plugin-dired)

;;; imenu / Imenuで概要参照
;;;--------------------------------------------------

(defvar e2wm:def-plugin-imenu-cached-entries nil)
(make-variable-buffer-local 'e2wm:def-plugin-imenu-cached-entries)
(defvar e2wm:def-plugin-imenu-cached-tick nil)
(make-variable-buffer-local 'e2wm:def-plugin-imenu-cached-tick)

(defun e2wm:def-plugin-imenu (frame wm winfo)
  (let ((entries (e2wm:def-plugin-imenu-entries))
        (wname (wlf:window-name winfo))
        (buf (wlf:window-option-get winfo :buffer)) pos)
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create " *WM:Imenu*"))
      (with-current-buffer buf
        (e2wm:def-plugin-imenu-mode)
        (setq buffer-read-only t)
        (setq truncate-lines t)
        (buffer-disable-undo buf)
        (hl-line-mode 1))
      (e2wm:def-plugin-imenu-start-timer))
    (with-current-buffer buf
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (setq pos (point))
            (erase-buffer)
            (loop for i in entries
                  do (insert i "\n"))
            (setq mode-line-format 
                  '("-" mode-line-mule-info
                    " " mode-line-position "-%-"))
            (setq header-line-format
                  (format "Imenu entries [%i]" (length entries)))
            (goto-char pos)
            (hl-line-highlight))
        (setq buffer-read-only t)))
    (wlf:set-buffer wm wname buf)
    (set-window-point (wlf:get-window wm wname) pos)))

(defun e2wm:def-plugin-imenu-entries ()
  "[internal] Return a list of imenu items to insert the imenu buffer."
  (with-current-buffer (e2wm:history-get-main-buffer)
    (let ((tick (buffer-modified-tick)))
      (if (and (eq e2wm:def-plugin-imenu-cached-tick tick)
               e2wm:def-plugin-imenu-cached-entries)
          e2wm:def-plugin-imenu-cached-entries
        (setq imenu--index-alist nil)
        (setq e2wm:def-plugin-imenu-cached-tick tick
              e2wm:def-plugin-imenu-cached-entries
              (condition-case nil
                  (nreverse
                   (e2wm:def-plugin-imenu-create-entries
                    (imenu--make-index-alist) "" nil))
                (error nil)))))))

(defun e2wm:def-plugin-imenu-create-entries (entries indent result)
  "[internal] Make a menu item from the imenu object and return a
string object to insert the imenu buffer."
  (loop for i in entries do
        (cond
         ;; item
         ((number-or-marker-p (cdr i))
          (let ((title (concat indent (car i)))
                (mark (cdr i)))
            (push (propertize title 'e2wm:imenu-mark mark) result)))
         ;; overlay
         ((overlayp (cdr i))
          (let ((title (concat indent (car i)))
                (mark (overlay-start (cdr i))))
            (push (propertize title 'e2wm:imenu-mark mark) result)))
         ;; cascade
         ((listp (cdr i))
          ;; title
          (push (e2wm:rt (concat indent (car i)) 'e2wm:face-subtitle) result)
          ;; contents
          (setq result
                (e2wm:def-plugin-imenu-create-entries
                 (cdr i) (concat "  " indent) result)))
         ;; ? 
         (t nil)))
  result)
  
(setq imenu-default-goto-function 'imenu-default-goto-function)

(defun e2wm:def-plugin-imenu-jump (elm)
  "[internal] Jump to the selected imenu item."
  (let ((mark (get-text-property 0 'e2wm:imenu-mark elm)))
    (when mark
      (push-mark)
      (imenu-default-goto-function elm mark))))

(defun e2wm:def-plugin-imenu-jump-command ()
  "Jump to the selected imenu item on the main window."
  (interactive)
  (let ((elm (e2wm:string-trim (thing-at-point 'line))))
    (select-window (get-buffer-window (e2wm:history-get-main-buffer)))
    (e2wm:def-plugin-imenu-jump elm)))

(defun e2wm:def-plugin-imenu-show-command ()
  "Show the selected imenu item on the main window."
  (interactive)
  (let ((elm (e2wm:string-trim (thing-at-point 'line)))
        (cwin (selected-window)))
    (select-window (get-buffer-window (e2wm:history-get-main-buffer)))
    (e2wm:def-plugin-imenu-jump elm)
    (select-window cwin)))

(defvar e2wm:def-plugin-imenu-mode-map 
  (e2wm:define-keymap 
   '(("C-m" . e2wm:def-plugin-imenu-jump-command)
     ("j" . next-line)
     ("k" . previous-line)
     ("n" . next-line)
     ("p" . previous-line)
     ("u" . scroll-down)
     ("e" . scroll-down)
     ("d" . scroll-up)
     ("v" . scroll-up)
     ("q" . e2wm:pst-window-select-main-command)
     ("<SPC>" . e2wm:def-plugin-imenu-show-command)
     )))

(define-derived-mode e2wm:def-plugin-imenu-mode fundamental-mode "Imenu")

(defun e2wm:def-plugin-imenu-which-func ()
  (loop with which-func = nil
        with minoffset = (point-max)
        for i in e2wm:def-plugin-imenu-cached-entries
        for mark = (get-text-property 0 'e2wm:imenu-mark i)
        if (number-or-marker-p mark)
        do (let ((offset (- (point) mark)))
             (if (>= offset 0)
                 (when (< offset minoffset)
                   (setq which-func i
                         minoffset offset))))
        finally return which-func))

(defvar e2wm:def-plugin-imenu-timer nil)

(defun e2wm:def-plugin-imenu-start-timer ()
  (interactive)
  (unless e2wm:def-plugin-imenu-timer
    (setq e2wm:def-plugin-imenu-timer
          (run-with-idle-timer 
           idle-update-delay t 
           'e2wm:def-plugin-imenu-update-which-func))
    (e2wm:message "Imenu timer started.")))

(defun e2wm:def-plugin-imenu-stop-timer ()
  (interactive)
  (when (timerp e2wm:def-plugin-imenu-timer)
    (cancel-timer e2wm:def-plugin-imenu-timer))
  (setq e2wm:def-plugin-imenu-timer nil)
  (e2wm:message "Imenu timer stopped."))

(defun e2wm:def-plugin-imenu-update-which-func ()
  (e2wm:with-advice
   (let* ((main-buf (e2wm:history-get-main-buffer))
          (win (selected-window))
          (imenu-buf (get-buffer " *WM:Imenu*"))
          (imenu-win (and imenu-buf (get-buffer-window imenu-buf))))
     (cond
      ((null imenu-buf)
       (e2wm:def-plugin-imenu-stop-timer))
      ((eql win (get-buffer-window main-buf))
       (let ((name (e2wm:def-plugin-imenu-which-func)))
         (when (and name (window-live-p imenu-win))
           (with-current-buffer imenu-buf
             (goto-char (point-min))
             (let ((ps (re-search-forward (concat "^" (regexp-quote name) "$"))))
               (when ps
                 (beginning-of-line)
                 (set-window-point imenu-win (point))
                 (hl-line-highlight)))))))
      (t
       ;;can not update
       )))))

(e2wm:plugin-register 'imenu
                     "Outline"
                     'e2wm:def-plugin-imenu)

;;; top / topでマシン状態表示
;;;--------------------------------------------------

;; 自動更新のデモ

(defvar e2wm:def-plugin-top-buffer-name " *WM:Top*" "[internal use]")
(defvar e2wm:def-plugin-top-timer-handle nil "[internal use]")
(defvar e2wm:def-plugin-top-timer-interval 20 "Seconds for update.")

(defun e2wm:def-plugin-top (frame wm winfo)
  ;;bufferが生きていればバッファを表示するだけ（タイマーに任せる）
  ;;bufferが無ければ初回更新してタイマー開始する
  (let ((buf (get-buffer e2wm:def-plugin-top-buffer-name)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (e2wm:def-plugin-top-update)))
    (unless e2wm:def-plugin-top-timer-handle
      (setq e2wm:def-plugin-top-timer-handle
            (run-at-time
             e2wm:def-plugin-top-timer-interval
             e2wm:def-plugin-top-timer-interval
             'e2wm:def-plugin-top-timer))
      (e2wm:message "WM: 'top' update timer started."))
    (wlf:set-buffer wm (wlf:window-name winfo) buf)))

(defun e2wm:def-plugin-top-timer ()
  ;;bufferが死んでいれば、タイマー停止
  ;;bufferが生きていれば更新実行
  (let ((buf (get-buffer e2wm:def-plugin-top-buffer-name)))
    (if (and (e2wm:managed-p) buf (buffer-live-p buf)
             (get-buffer-window buf))
        (when (= 0 (minibuffer-depth))
          (e2wm:def-plugin-top-update))
      (when e2wm:def-plugin-top-timer-handle
          (cancel-timer e2wm:def-plugin-top-timer-handle)
          (setq e2wm:def-plugin-top-timer-handle nil)
          (e2wm:message "WM: 'top' update timer stopped.")))))

(defun e2wm:def-plugin-top-update ()
  (lexical-let* ((buf (get-buffer e2wm:def-plugin-top-buffer-name))
                 (tmpbuf (get-buffer-create " *WM:Top-temp*"))
                 (cleanup (lambda() (kill-buffer tmpbuf))))
    (buffer-disable-undo tmpbuf)
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create e2wm:def-plugin-top-buffer-name))
      (with-current-buffer buf
        (buffer-disable-undo buf)))
    (let (proc)
      (condition-case err
          (setq proc (start-process "WM:top" tmpbuf "top" "-b" "-n" "1"))
        (nil 
         (with-current-buffer buf
           (erase-buffer)
           (insert (error-message-string err))
           (funcall cleanup))))
      (when proc
        (set-process-sentinel
         proc (lambda(proc event)
                (with-current-buffer buf
                  (erase-buffer)
                  (if (equal event "finished\n")
                      (insert-buffer-substring tmpbuf)
                    (insert "Error: Can not use top output.\n" 
                            (format "%s" event)))
                  (goto-char (point-min))
                  (funcall cleanup))
                ))))
    buf))

(e2wm:plugin-register 'top 
                     "Top (System Stat)"
                     'e2wm:def-plugin-top)

;;; history-nth / 履歴のN番目を表示
;;;--------------------------------------------------

;; 順番については以下のようにオプションで指定
;; 1でメインと同じ。2で1つ前（デフォルト）。
;; 例 (:name window-name :plugin history-nth :plugin-args 2)

(defun e2wm:def-plugin-history-nth (frame wm winfo)
  (let* ((index (1- (or (wlf:window-option-get winfo :plugin-args) 2)))
         (buf (or (nth index (e2wm:history-get)) 
                  (e2wm:history-get-main-buffer))))
    (when buf
      (wlf:set-buffer wm (wlf:window-name winfo) buf))))

(e2wm:plugin-register 'history-nth 
                     "History Back Buffer"
                     'e2wm:def-plugin-history-nth)

;;; main-prev / ひとつ前にメインに表示していたバッファを表示
;;;--------------------------------------------------

;; history-nthのとの違いは、undoキューがあればそっちの頭を表示する。

(defun e2wm:def-plugin-main-prev (frame wm winfo)
  (let* ((buf (or (car (e2wm:history-get-backup))
                  (nth 1 (e2wm:history-get)) 
                  (e2wm:history-get-main-buffer))))
    (when buf
      (wlf:set-buffer wm (wlf:window-name winfo) buf))))

(e2wm:plugin-register 'main-prev 
                     "Previous Buffer"
                     'e2wm:def-plugin-main-prev)

;;; clock / 時計
;;;--------------------------------------------------

(defvar e2wm:def-plugin-clock-timer-interval 60 "Seconds for update.")
(defvar e2wm:def-plugin-clock-text nil)
(defvar e2wm:def-plugin-clock-url "http://www.bijint.com/jp/img/clk/%H%M.jpg" "URL pattern.")
(defvar e2wm:def-plugin-clock-referer "http://www.bijint.com/jp/" "referer URL")

(defvar e2wm:def-plugin-clock-buffer-name " *WM:Clock*" "[internal use]")
(defvar e2wm:def-plugin-clock-timer-handle nil "[internal use]")
(defvar e2wm:def-plugin-clock-window nil "[internal use] Display window.") ; 表示するウインドウは1つであることを仮定（サイズ取得のため）
(defvar e2wm:def-plugin-clock-download-file "/tmp/wmclock.jpg"  "[internal]")
(defvar e2wm:def-plugin-clock-resized-file  "/tmp/wmclockt.jpg" "[internal]")
;;↑cygwin環境の場合は "C:/cygwin/tmp/wmclock.jpg" とかにすると良いかも

(defun e2wm:def-plugin-clock (frame wm winfo)
  ;;bufferが生きていればバッファを表示するだけ（タイマーに任せる）
  ;;bufferが無ければ初回更新してタイマー開始する
  (let ((buf (get-buffer e2wm:def-plugin-clock-buffer-name)))
    (setq e2wm:def-plugin-clock-window 
          (wlf:get-window wm (wlf:window-name winfo)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (e2wm:def-plugin-clock-update)))
    (unless e2wm:def-plugin-clock-timer-handle
      (setq e2wm:def-plugin-clock-timer-handle
            (run-at-time
             e2wm:def-plugin-clock-timer-interval
             e2wm:def-plugin-clock-timer-interval
             'e2wm:def-plugin-clock-timer))
      (e2wm:message "WM: 'clock' update timer started."))
    (wlf:set-buffer wm (wlf:window-name winfo) buf)))

(defun e2wm:def-plugin-clock-timer ()
  ;;bufferが死んでいれば、タイマー停止
  ;;bufferが生きていれば更新実行
  (let ((buf (get-buffer e2wm:def-plugin-clock-buffer-name)))
    (if (and (e2wm:managed-p) buf (buffer-live-p buf) 
             (get-buffer-window buf))
        (when (= 0 (minibuffer-depth))
          (e2wm:def-plugin-clock-update))
      (when e2wm:def-plugin-clock-timer-handle
          (cancel-timer e2wm:def-plugin-clock-timer-handle)
          (setq e2wm:def-plugin-clock-timer-handle nil)
          (when buf (kill-buffer buf))
          (e2wm:message "WM: 'clock' update timer stopped.")))))

(defun e2wm:def-plugin-clock-update ()
  (let ((buf (get-buffer e2wm:def-plugin-clock-buffer-name)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create e2wm:def-plugin-clock-buffer-name))
      (with-current-buffer buf
        (buffer-disable-undo buf)
        (setq mode-line-format 
              '("-" mode-line-mule-info
                " E2wm Clock -%-"))))
    (if e2wm:def-plugin-clock-text
        (e2wm:def-plugin-clock-show-text "Text mode")
      (e2wm:def-plugin-clock-download))
    buf))

(defun e2wm:def-plugin-clock-download ()
  (lexical-let ((tmpbuf (get-buffer-create " *WM:Clock-temp*")))
    (buffer-disable-undo tmpbuf)
    (let* ((url (format-time-string 
                 e2wm:def-plugin-clock-url
                  (current-time)))
           (proc (start-process
                  "WM:clockw" tmpbuf "wget"
                  (concat "--referer=" e2wm:def-plugin-clock-referer)
                  "-q" "-O" e2wm:def-plugin-clock-download-file url)))
      (set-process-sentinel
       proc (lambda(proc event)
              (cond 
               ((string-match "exited abnormally" event)
                (kill-buffer tmpbuf)
                (e2wm:def-plugin-clock-show-text "No network connection."))
               ((equal event "finished\n")
                (kill-buffer tmpbuf)
                (let ((f e2wm:def-plugin-clock-download-file))
                  (if (and (file-exists-p f)
                           (< 0 (nth 7 (file-attributes f))))
                      (e2wm:def-plugin-clock-resize)
                    (e2wm:def-plugin-clock-show-text "No network connection."))))))))))

(defun e2wm:def-plugin-clock-resize ()
  (lexical-let* 
      ((tmpbuf (get-buffer-create " *WM:Clock-temp*")) 
       (window e2wm:def-plugin-clock-window)
       (w (* (window-width window) (frame-char-width)))
       (h (* (- (window-height window) 1) (frame-char-height)))
       (proc
        (start-process "WM:clockc" tmpbuf "convert" 
                       "-resize" 
                       (format "%ix%i" w h)
                       e2wm:def-plugin-clock-download-file 
                       (concat "jpeg:" e2wm:def-plugin-clock-resized-file))))
    (set-process-sentinel
     proc (lambda (proc event)
            (cond
               ((string-match "exited abnormally" event)
                (kill-buffer tmpbuf)
                (e2wm:def-plugin-clock-show-text "Could not convert."))
               ((equal event "finished\n")
                (kill-buffer tmpbuf)
                (let ((f e2wm:def-plugin-clock-resized-file))
                  (if (and (file-exists-p f)
                           (< 0 (nth 7 (file-attributes f))))
                      (e2wm:def-plugin-clock-show-image)
                    (e2wm:def-plugin-clock-show-text "Could not convert."))))
              )))))

(defun e2wm:def-plugin-clock-show-image ()
  (clear-image-cache)
  (let ((buf (get-buffer e2wm:def-plugin-clock-buffer-name))
        (img (create-image e2wm:def-plugin-clock-resized-file 'jpeg))
        (map (make-sparse-keymap)))
    (define-key map [mouse-1] 'e2wm:def-plugin-clock-onclick)
    (with-current-buffer buf
      (erase-buffer)
      (goto-char (point-min))
      (insert "clock image")
      (add-text-properties 
       (point-min) (point-max)
       (list 'display img 'keymap map 'mouse-face 'highlight)))))

(defun e2wm:def-plugin-clock-onclick ()
  (interactive)
  (browse-url e2wm:def-plugin-clock-referer))

(defun e2wm:def-plugin-clock-show-text (&optional text)
  (let ((buf (get-buffer e2wm:def-plugin-clock-buffer-name))
        (time (current-time)))
    (with-current-buffer buf
      (erase-buffer)
      (goto-char (point-min))
      (when text
        (insert 
         (e2wm:rt-format "Status: %s\n" text)))
      (insert 
       (e2wm:rt-format
        "\nSystem: %s\nLoad Average: %s\n\n"
        (system-name) 
        (mapconcat 'identity (loop for i in (load-average t)
                                   collect (format "%.2f" i)) ", ")))
      (insert 
       (e2wm:rt-format
        "Date: %s\nTime: %s\n"
        (format-time-string "%Y/%m/%d" time)
        (cons (format-time-string "%H:%M" time) 'e2wm:face-title)))
    )))

(e2wm:plugin-register 'clock 
                     "Fancy Clock"
                     'e2wm:def-plugin-clock)

;;; files / シンプルなファイル一覧
;;;--------------------------------------------------
;; 例 (:name window-name :plugin files) ;デフォルト
;;    (:name window-name :plugin files 
;;     :plugin-args (:sort time :show-hidden t)) ; 設定付き
;; buffer-local : e2wm:def-plugin-files-dir
;;                e2wm:def-plugin-files-sort-key
;;                e2wm:def-plugin-files-hide-hidden-files

(defvar e2wm:def-plugin-files-dir nil "[internal buffer local]")
(defvar e2wm:def-plugin-files-sort-key nil "[internal buffer local]")
(defvar e2wm:def-plugin-files-hide-hidden-files nil "[internal buffer local]")
 
(defun e2wm:def-plugin-files (frame wm winfo)
  (let* ((buf (e2wm:history-get-main-buffer))
         (wname (wlf:window-name winfo))
         (opts (wlf:window-option-get winfo :plugin-args))
         (opt-sort-key (or (plist-get opts ':sort) 'name))
         (opt-hide-hidden (not (plist-get opts ':show-hidden)))
         (dir (with-current-buffer buf 
                (or default-directory ".")))
         (buf-name (format " *WM:Files-%s*" wname))
         (dbuf (get-buffer buf-name)) pos)
    (unless (and dbuf (buffer-live-p dbuf))
      (setq dbuf (get-buffer-create buf-name))
      (with-current-buffer dbuf
        (e2wm:def-plugin-files-mode)
        (set (make-local-variable 'e2wm:def-plugin-files-dir) dir)
        (set (make-local-variable 'e2wm:def-plugin-files-sort-key) opt-sort-key)
        (set (make-local-variable 'e2wm:def-plugin-files-hide-hidden-files) opt-hide-hidden)
        (setq buffer-read-only t)
        (setq truncate-lines t)
        (buffer-disable-undo dbuf)
        (setq pos (point-min))
        (hl-line-mode 1)))
    (with-current-buffer dbuf
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (setq pos 
                  (if (and e2wm:def-plugin-files-dir
                           (equal e2wm:def-plugin-files-dir dir))
                      (point) (point-min)))
            (setq e2wm:def-plugin-files-dir dir)
            (erase-buffer)
            (e2wm:def-plugin-files-update-buffer dir)
            (goto-char pos))
        (setq buffer-read-only t)))
    (wlf:set-buffer wm wname dbuf)))

(e2wm:plugin-register 'files 
                     "Files"
                     'e2wm:def-plugin-files)

(defface e2wm:face-files-main
  '((t (:inherit font-lock-constant-face)))
  "Face used for main info."
  :group 'e2wm-files)
(defface e2wm:face-files-symlink
  '((t (:inherit font-lock-keyword-face)))
  "Face used for symbolic links."
  :group 'e2wm-files)
(defface e2wm:face-files-directory
  '((t (:inherit font-lock-function-name-face)))
  "Face used for subdirectories."
  :group 'e2wm-files)
(defface e2wm:face-files-shadow
  '((t (:inherit shadow)))
  "Face used for shadowed info."
  :group 'e2wm-files)

(defun e2wm:def-plugin-files-update-by-command()
  (interactive)
  ;;カレントバッファが対象のバッファである前提
  (when (eq major-mode 'e2wm:def-plugin-files-mode)
    (unwind-protect
        (progn
          (setq buffer-read-only nil)
          (erase-buffer)
          (e2wm:def-plugin-files-update-buffer e2wm:def-plugin-files-dir)
          (goto-char (point-min)))
      (setq buffer-read-only t))))

(defun e2wm:def-plugin-files-sort (records order)
  (let* 
      ((comparator
        (lambda (ref)
          (lexical-let ((ref ref))
            (lambda (i j) 
              (let ((ii (nth ref i))
                    (jj (nth ref j)))
                (cond 
                 ((string= ii jj) 0)
                 ((string< ii jj) -1)
                 (t 1)))))))
       (negative-comparator
        (lambda (ref)
          (lexical-let ((ref ref))
            (lambda (i j) 
              (let ((ii (nth ref i))
                    (jj (nth ref j)))
                (cond 
                 ((string= ii jj) 0)
                 ((string< ii jj) 1)
                 (t -1)))))))
       (to-bool 
        (lambda (f)
          (lexical-let ((f f))
            (lambda (i j) 
              (< (funcall f i j) 0)))))
       (cmp-name (funcall comparator 0))
       (cmp-dir  (funcall comparator 4))
       (cmp-time (funcall negative-comparator 2))
       (cmp-size (funcall negative-comparator 5))
       (chain 
        (lambda (a b)
          (lexical-let ((a a) (b b))
            (lambda (i j)
              (let ((v (funcall a i j)))
                (if (= 0 v)
                    (funcall b i j)
                  v)))))))
    (nreverse (sort 
               records
               (cond
                ((eq order 'name) ; name -> dir, name
                 (funcall to-bool (funcall chain cmp-dir cmp-name)))
                ((eq order 'time) ; time -> dir, time
                 (funcall to-bool cmp-time))
                ((eq order 'size) ; size -> dir, size
                 (funcall to-bool (funcall chain cmp-dir cmp-size)))
                (t  ; default
                 (funcall to-bool (funcall chain cmp-dir cmp-name))))))))

(defun e2wm:def-plugin-files-update-buffer (dir)
  (let* ((files 
          (e2wm:def-plugin-files-sort
           (loop 
            for f in (directory-files-and-attributes dir)
            for fn = (car f)
            unless (or (member fn '(".." "."))
                       (and e2wm:def-plugin-files-hide-hidden-files
                            (eq (aref fn 0) ?.)))
            collect (list 
                     ;; 0:name, 1:dirp, 2:time, 3:size
                     ;; 4:dirp-str, 5:size-str, 6:float-time
                     fn (cadr f) 
                     (format-time-string "%Y/%m/%d %H:%M:%S" (nth 7 f))
                     (nth 8 f)
                     (if (cadr f) "d" "f")
                     (if (nth 8 f) (format "%014d" (nth 8 f)) (make-string 14 ?\ ))
                     (float-time (nth 7 f))))
           e2wm:def-plugin-files-sort-key)) rows-file rows-time rows-size rows)
    (loop for i in files
          for fn = (substring (car i) 0) 
          for tm = (nth 2 i) for sz = (nth 3 i)
          for type = (cadr i)
          do
          (push i rows)
          (push 
           (e2wm:tp 
            (cond
             ((stringp type) (e2wm:rt fn 'e2wm:face-files-symlink))
             (type (e2wm:rt fn 'e2wm:face-files-directory))
             (t fn))
            'e2wm:file
            (expand-file-name fn dir)) rows-file)
          (push (nth 2 i) rows-time)
          (push (e2wm:format-byte-unit (nth 3 i)) rows-size))
    (cond
     ((eq e2wm:def-plugin-files-sort-key 'name)
      (e2wm:def-plugin-files-insert-by-name rows-file rows-time rows-size))
     ((eq e2wm:def-plugin-files-sort-key 'time)
      (e2wm:def-plugin-files-insert-by-time rows-file rows-time rows-size rows))
     ((eq e2wm:def-plugin-files-sort-key 'size)
      (e2wm:def-plugin-files-insert-by-size rows-file rows-time rows-size)))
    (setq mode-line-format 
          '("-" mode-line-mule-info
            " " mode-line-position "-%-"))
    (let* ((win (get-buffer-window (current-buffer)))
           (width (- (or (and win (window-width win)) 90) 7)) ; 9 <= num and ellipse "..."
           (dirname (expand-file-name dir))
           (namelen (string-width dirname))
           (startcol (max 0 (- namelen width))))
      (setq header-line-format
            (format "[%2i] %s%s"
                    (length files) (if (< 0 startcol) "..." "")
                    (truncate-string-to-width dirname namelen startcol))))))

(defun e2wm:def-plugin-files-insert-by-name (rows-file rows-time rows-size)
  (loop for i from 0 below (length rows-file)
        with wfile = (e2wm:max-length rows-file)
        with wtime = (e2wm:max-length rows-time)
        with fmt = (format "%%-%is  %%%is  %%s\n" wfile wtime)
        for fn = (pop rows-file)
        for tm = (e2wm:rt (pop rows-time) 'e2wm:face-files-shadow)
        for sz = (e2wm:rt (pop rows-size) 'e2wm:face-files-shadow)
        do
        (insert 
         (format fmt fn tm sz))))

(defun e2wm:def-plugin-files-insert-by-time (rows-file rows-time rows-size rows)
  (let* ((today (apply 'encode-time 
                       (append '(0 0 0)
                               (cdddr (decode-time
                                       (current-time))))))
         (ftoday  (float-time today))
         (fyesterday  (- ftoday 86400))
         (flast-week  (- ftoday (* 86400 7)))
         (flast-month (- ftoday (* 86400 30))))
    (loop for i from 0 below (length rows-file)
          with wfile = (e2wm:max-length rows-file)
          with wtime = (e2wm:max-length rows-time)
          with today-first = nil
          with last-ftime = (float-time (current-time))
          with fmt = (format "%%-%is  %%%is  %%s\n" wfile wtime)
          with spc = (make-string 10 ?-)
          for fn = (pop rows-file)
          for tm = (e2wm:rt (pop rows-time) 'e2wm:face-files-main)
          for sz = (e2wm:rt (pop rows-size) 'e2wm:face-files-shadow)
          for ftm = (nth 6 (pop rows))
          for splitter =
          (cond 
           ((and (> last-ftime flast-month) (> flast-month ftm))
            "More")
           ((and (> last-ftime flast-week) (> flast-week ftm))
            "One Month")
           ((and (> last-ftime fyesterday) (> fyesterday ftm))
            "Last Week")
           ((and (> last-ftime ftoday) (> ftoday ftm))
            "Yesterday")
           ((and (null today-first) (< ftoday ftm))
            (setq today-first t)
            "Today")
           (t nil))
          do
          (when splitter
            (insert (e2wm:rt (concat spc (format "- %s ---------\n" splitter))
                            'e2wm:face-files-shadow)))
          (insert 
           (format fmt fn tm sz))
          (setq last-ftime ftm))))

(defun e2wm:def-plugin-files-insert-by-size (rows-file rows-time rows-size)
  (loop for i from 0 below (length rows-file)
        with wfile = (e2wm:max-length rows-file)
        with wsize = (e2wm:max-length rows-size)
        with fmt = (format "%%%is  %%-%is  %%s\n" wsize wfile)
        for fn = (pop rows-file)
        for tm = (e2wm:rt (pop rows-time) 'e2wm:face-files-shadow)
        for sz = (e2wm:rt (pop rows-size) 'e2wm:face-files-main)
        do
        (insert 
         (format fmt sz fn tm))))

(defun e2wm:def-plugin-files-get-file ()
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) 'e2wm:file)))

(defun e2wm:def-plugin-files-mkdir-command ()
  (interactive)
  (let ((dir 
         (read-file-name "mkdir: " e2wm:def-plugin-files-dir)))
    (make-directory dir)
    (e2wm:def-plugin-files-update-by-command)))
(defun e2wm:def-plugin-files-delete-command ()
  (interactive)
  (let ((file (e2wm:def-plugin-files-get-file)))
    (when (yes-or-no-p (format "Delete [%s] ? " file))
      (cond
       ((file-directory-p file)
        (delete-directory file))
       (t
        (delete-file file)))
      (e2wm:def-plugin-files-update-by-command))))
(defun e2wm:def-plugin-files-updir-command ()
  (interactive)
  (when (eq major-mode 'e2wm:def-plugin-files-mode)
    (setq e2wm:def-plugin-files-dir 
          (expand-file-name ".." e2wm:def-plugin-files-dir))
    (e2wm:def-plugin-files-update-by-command)))
(defun e2wm:def-plugin-files-rename-command ()
  (interactive)
  (let ((file (e2wm:def-plugin-files-get-file)))
    (e2wm:aif
        (read-file-name (format "Rename [%s] to: " file) e2wm:def-plugin-files-dir)
        (progn
          (rename-file file it)
          (e2wm:def-plugin-files-update-by-command)))))
(defun e2wm:def-plugin-files-sort-size-command ()
  (interactive)
  (when (eq major-mode 'e2wm:def-plugin-files-mode)
    (setq e2wm:def-plugin-files-sort-key 'size)
    (e2wm:def-plugin-files-update-by-command)))
(defun e2wm:def-plugin-files-sort-time-command ()
  (interactive)
  (when (eq major-mode 'e2wm:def-plugin-files-mode)
    (setq e2wm:def-plugin-files-sort-key 'time)
    (e2wm:def-plugin-files-update-by-command)))
(defun e2wm:def-plugin-files-sort-name-command ()
  (interactive)
  (when (eq major-mode 'e2wm:def-plugin-files-mode)
    (setq e2wm:def-plugin-files-sort-key 'name)
    (e2wm:def-plugin-files-update-by-command)))
(defun e2wm:def-plugin-files-show-command ()
  (interactive)
  (let ((cwin (selected-window)))
    (e2wm:def-plugin-files-select-command)
    (select-window cwin)))
(defun e2wm:def-plugin-files-select-command ()
  (interactive)
  (let ((file (e2wm:def-plugin-files-get-file)))
    (cond
     ((file-directory-p file)
      (setq e2wm:def-plugin-files-dir file)
      (e2wm:def-plugin-files-update-by-command))
     (t
      (e2wm:history-add (find-file-noselect file))
      (e2wm:pst-show-history-main)
      (e2wm:pst-window-select-main)))))
(defun e2wm:def-plugin-files-toggle-hidden-files-command ()
  (interactive)
  (when (eq major-mode 'e2wm:def-plugin-files-mode)
    (setq e2wm:def-plugin-files-hide-hidden-files
          (not e2wm:def-plugin-files-hide-hidden-files))
    (e2wm:def-plugin-files-update-by-command)))
(defun e2wm:def-plugin-files-open-dired-command ()
  (interactive)
  (dired e2wm:def-plugin-files-dir))

(defvar e2wm:def-plugin-files-mode-map 
  (e2wm:define-keymap 
   '(
     ("k" . previous-line)
     ("j" . next-line)
     ("p" . previous-line)
     ("n" . next-line)
     ("D" . e2wm:def-plugin-files-open-dired-command)
     ("h" . e2wm:def-plugin-files-toggle-hidden-files-command)
     ("+" . e2wm:def-plugin-files-mkdir-command)
     ("g" . e2wm:def-plugin-files-update-by-command)
     ("d" . e2wm:def-plugin-files-delete-command)
     ("^" . e2wm:def-plugin-files-updir-command)
     ("r" . e2wm:def-plugin-files-rename-command)
     ("t" . e2wm:def-plugin-files-sort-time-command)
     ("s" . e2wm:def-plugin-files-sort-name-command)
     ("z" . e2wm:def-plugin-files-sort-size-command)
     ("q" . e2wm:pst-window-select-main-command)
     ("<SPC>" . e2wm:def-plugin-files-show-command)
     ("C-m"   . e2wm:def-plugin-files-select-command)
     )))

(define-derived-mode e2wm:def-plugin-files-mode fundamental-mode "Files")

;;; open / バッファ表示・コマンド実行
;;; 指定のバッファを表示バッファの存在をチェックして、無かったらコマンドを実行
;;;--------------------------------------------------
;; w3m や twittering-mode などを起動
;; 例 (:name window-name :plugin open :plugin-args (:buffer "*w3m*" :command 'w3m))
;;  :buffer  : 表示すべきバッファ。存在してなければコマンドを実行する。lambdaでも可。
;;  :command : 実行すべき関数のシンボル。関数の返値はバッファオブジェクト。

(defun e2wm:def-plugin-open (frame wm winfo)
  (let* ((plugin-args (wlf:window-option-get winfo :plugin-args))
         (buffer-name (plist-get plugin-args ':buffer))
         (command (plist-get plugin-args ':command)) buf)
    (unless (and command buffer-name)
      (error "e2wm:plugin open: arguments can not be nil. Check the options."))
    (setq buf (get-buffer buffer-name))
    (unless buf
      (with-selected-window (wlf:get-window wm (wlf:window-name winfo))
        (setq buf (funcall command))))
    (when buf
      (wlf:set-buffer wm (wlf:window-name winfo) buf))))

(e2wm:plugin-register 'open
                     "Open Buffer"
                     'e2wm:def-plugin-open)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Perspective Definition

;;; base / A base class for perspectives
;;;--------------------------------------------------

(e2wm:pst-class-register
  (make-e2wm:$pst-class
   :name   'base
   :update 'e2wm:dp-base-update))

(defun e2wm:dp-base-update (wm)
  ;;プラグイン更新実行
  (e2wm:plugin-exec-update (selected-frame) wm))


;;; code / Code editing perspective
;;;--------------------------------------------------

(defvar e2wm:c-code-recipe
  '(| (:left-max-size 35)
      (- (:upper-size-ratio 0.7)
         files history)
      (- (:upper-size-ratio 0.7)
         (| (:right-max-size 30)
            main imenu)
         sub)))

(defvar e2wm:c-code-winfo
  '((:name main)
    (:name files :plugin files)
    (:name history :plugin history-list)
    (:name sub :buffer "*info*" :default-hide t)
    (:name imenu :plugin imenu :default-hide nil))
  )

(defvar e2wm:c-code-show-main-regexp
   "\\*\\(vc-diff\\)\\*")

(e2wm:pst-class-register 
  (make-e2wm:$pst-class
   :name   'code
   :extend 'base
   :title  "Coding"
   :init   'e2wm:dp-code-init
   :main   'main
   :switch 'e2wm:dp-code-switch
   :popup  'e2wm:dp-code-popup
   :keymap 'e2wm:dp-code-minor-mode-map))

(defun e2wm:dp-code-init ()
  (let* 
      ((code-wm 
        (wlf:no-layout 
         e2wm:c-code-recipe
         e2wm:c-code-winfo))
       (buf (or e2wm:prev-selected-buffer
                (e2wm:history-get-main-buffer))))

    (when (e2wm:history-recordable-p e2wm:prev-selected-buffer)
      (e2wm:history-add e2wm:prev-selected-buffer))
    
    (wlf:set-buffer code-wm 'main buf)
    code-wm))

(defun e2wm:dp-code-switch (buf)
  (e2wm:message "#DP CODE switch : %s / %S" buf (e2wm:history-recordable-p buf))
  (if (e2wm:history-recordable-p buf)
      (progn
        (e2wm:pst-show-history-main)
        (e2wm:pst-window-select-main))
    nil))

(defun e2wm:dp-code-popup (buf)
  ;;とりあえず全部subで表示してみる
  (let ((cb (current-buffer)))
    (e2wm:message "#DP CODE popup : %s (current %s / backup %s)" 
                 buf cb e2wm:override-window-cfg-backup))
  (let ((buf-name (buffer-name buf))
        (wm (e2wm:pst-get-wm)))
    (cond
     ((e2wm:history-recordable-p buf)
      (e2wm:pst-show-history-main)
      ;;記録対象なら履歴に残るのでupdateで表示を更新させる
      t)
     ((and e2wm:override-window-cfg-backup
       (eq (selected-window) (wlf:get-window wm 'sub)))
      ;;現在subならmainに表示しようとする
      ;;minibuffer以外の補完バッファは動きが特殊なのでbackupをnilにする
      (setq e2wm:override-window-cfg-backup nil)
      ;;一時的に表示するためにset-window-bufferを使う
      ;;(prefix) C-lなどで元のバッファに戻すため
      (set-window-buffer (wlf:get-window wm 'main) buf)
      t)
     ((and e2wm:c-code-show-main-regexp
           (string-match e2wm:c-code-show-main-regexp buf-name))
      (e2wm:pst-buffer-set 'main buf t)
      t)
     (t
      (e2wm:dp-code-popup-sub buf)
      t))))

(defun e2wm:dp-code-popup-sub (buf)
  (let ((wm (e2wm:pst-get-wm))
        (not-minibufp (= 0 (minibuffer-depth))))
    (e2wm:with-advice
     (e2wm:pst-buffer-set 'sub buf t not-minibufp))))

;; Commands / Keybindings

(defun e2wm:dp-code ()
  (interactive)
  (e2wm:pst-change 'code))

(defun e2wm:dp-code-imenu-toggle-command ()
  (interactive)
  (wlf:toggle (e2wm:pst-get-wm) 'imenu)
  (e2wm:pst-update-windows))
(defun e2wm:dp-code-sub-toggle-command ()
  (interactive)
  (wlf:toggle (e2wm:pst-get-wm) 'sub)
  (e2wm:pst-update-windows))
(defun e2wm:dp-code-navi-main-command ()
  (interactive)
  (wlf:select (e2wm:pst-get-wm) 'main))
(defun e2wm:dp-code-navi-files-command ()
  (interactive)
  (wlf:select (e2wm:pst-get-wm) 'files))
(defun e2wm:dp-code-navi-history-command ()
  (interactive)
  (wlf:select (e2wm:pst-get-wm) 'history))
(defun e2wm:dp-code-navi-imenu-command ()
  (interactive)
  (wlf:select (e2wm:pst-get-wm) 'imenu))
(defun e2wm:dp-code-navi-sub-command ()
  (interactive)
  (wlf:select (e2wm:pst-get-wm) 'sub))
(defun e2wm:dp-code-main-maximize-toggle-command ()
  (interactive)
  (wlf:toggle-maximize (e2wm:pst-get-wm) 'main))

(defun e2wm:dp-code-toggle-clock-command ()
  (interactive)
  (let* ((wm (e2wm:pst-get-wm))
         (prev (e2wm:pst-window-plugin-get wm 'history))
         (next (if (eq prev 'history-list)
                   'clock 'history-list)))
    (e2wm:pst-window-plugin-set wm 'history next)
    (e2wm:pst-update-windows)))

(defvar e2wm:dp-code-minor-mode-map 
      (e2wm:define-keymap
       '(("prefix I" . e2wm:dp-code-imenu-toggle-command)
         ("prefix S" . e2wm:dp-code-sub-toggle-command)
         ("prefix C" . e2wm:dp-code-toggle-clock-command)
         ("prefix M" . e2wm:dp-code-main-maximize-toggle-command))
       e2wm:prefix-key))

;;; two / Two column editing perspective
;;;--------------------------------------------------

(defvar e2wm:c-two-recipe
      '(- (:upper-size-ratio 0.8)
          (| left
             (- (:upper-size-ratio 0.9)
                right history))
          sub))

(defvar e2wm:c-two-winfo
      '((:name left )
        (:name right )
        (:name sub :buffer "*Help*" :default-hide t)
        (:name history :plugin history-list2 :default-hide nil)))

(defvar e2wm:c-two-right-default 'left) ; left, prev

(e2wm:pst-class-register
  (make-e2wm:$pst-class
   :name   'two
   :extend 'base
   :title  "Two Columns"
   :init   'e2wm:dp-two-init
   :main   'left
   :switch 'e2wm:dp-two-switch
   :popup  'e2wm:dp-two-popup
   :keymap 'e2wm:dp-two-minor-mode-map))

(defun e2wm:dp-two-init ()
  (let* 
      ((two-wm 
        (wlf:no-layout 
         e2wm:c-two-recipe
         e2wm:c-two-winfo))
       (buf (or e2wm:prev-selected-buffer
                (e2wm:history-get-main-buffer))))

    (wlf:set-buffer two-wm 'left buf)
    (cond
     ((eq e2wm:c-two-right-default 'left)
      (wlf:set-buffer two-wm 'right buf))
     ((eq e2wm:c-two-right-default 'prev)
      (wlf:set-buffer two-wm 'right (e2wm:history-get-prev buf)))
     (t
      (wlf:set-buffer two-wm 'right (e2wm:history-get-prev buf))))

    two-wm))

(defun e2wm:dp-two-switch (buf)
  (e2wm:message "#DP TWO switch : %s" buf)
  (let ((wm (e2wm:pst-get-wm))
        (curwin (selected-window)))
    (cond
     ((eql curwin (wlf:get-window wm 'left))
      ;; left画面の場合
      (cond 
       ((eql (get-buffer buf) (wlf:get-buffer wm 'left))
        ;; leftと同じなら並べる
        (e2wm:pst-update-windows)
        (e2wm:pst-buffer-set 'right buf)
        t)
       ((e2wm:history-recordable-p buf)
        ;; 普通の編集対象なら履歴につっこんで更新
        (e2wm:pst-show-history-main)
        t)
       (t 
        ;; それ以外ならとりあえず表示してみる
        nil)))

     ((eql curwin (wlf:get-window wm 'right))
      ;; right画面の場合
      (e2wm:pst-buffer-set 'right buf)
      (e2wm:dp-two-update-history-list)
      nil)
     (t nil))))

(defun e2wm:dp-two-popup (buf)
  ;;記録バッファ以外はsubで表示してみる
  (e2wm:message "#DP TWO popup : %s" buf)
  (let ((buf-name (buffer-name buf)))
    (cond
     ((e2wm:document-buffer-p buf)
      (e2wm:pst-buffer-set 'right buf)
      t)
     ((e2wm:history-recordable-p buf)
      (e2wm:pst-show-history-main)
      t)
     (t
      (e2wm:dp-two-popup-sub buf)
      t))))

(defun e2wm:dp-two-popup-sub (buf)
  (let ((wm (e2wm:pst-get-wm))
        (not-minibufp (= 0 (minibuffer-depth))))
    (e2wm:with-advice
     (e2wm:pst-buffer-set 'sub buf t not-minibufp))))

;; Commands / Keybindings

(defun e2wm:dp-two ()
  (interactive)
  (e2wm:pst-change 'two))

(defun e2wm:dp-two-navi-left-command ()
  (interactive)
  (e2wm:pst-window-select 'left))
(defun e2wm:dp-two-navi-right-command ()
  (interactive)
  (e2wm:pst-window-select 'right))
(defun e2wm:dp-two-navi-sub-command ()
  (interactive)
  (e2wm:pst-window-select 'sub))
(defun e2wm:dp-two-navi-history-command ()
  (interactive)
  (e2wm:pst-window-select 'history))

(defun e2wm:dp-two-history-toggle-command ()
  (interactive)
  (wlf:toggle (e2wm:pst-get-wm) 'history)
  (e2wm:pst-update-windows))
(defun e2wm:dp-two-sub-toggle-command ()
  (interactive)
  (wlf:toggle (e2wm:pst-get-wm) 'sub)
  (e2wm:pst-update-windows))

(defun e2wm:dp-two-update-history-list ()
  (e2wm:plugin-exec-update-by-plugin-name
   (selected-frame) (e2wm:pst-get-wm) 'history-list2))

(defun e2wm:dp-two-double-column-command ()
  (interactive)
  (e2wm:pst-buffer-set 'right (e2wm:history-get-main-buffer))
  (e2wm:dp-two-update-history-list))

(defun e2wm:dp-two-right-history-forward-command ()
  (interactive)
  (e2wm:pst-buffer-set
   'right (e2wm:history-get-next
           (e2wm:pst-buffer-get 'right)))
  (e2wm:dp-two-update-history-list))

(defun e2wm:dp-two-right-history-back-command ()
  (interactive)
  (e2wm:pst-buffer-set
   'right (e2wm:history-get-prev
           (e2wm:pst-buffer-get 'right)))
  (e2wm:dp-two-update-history-list))

(defalias 'e2wm:dp-two-right-history-up-command 'e2wm:dp-two-right-history-forward-command)
(defalias 'e2wm:dp-two-right-history-down-command 'e2wm:dp-two-right-history-back-command)

(defun e2wm:dp-two-swap-buffers-command ()
  (interactive)
  (let ((left  (e2wm:pst-buffer-get 'left))
        (right (e2wm:pst-buffer-get 'right)))
    (e2wm:pst-buffer-set 'left  right)
    (e2wm:pst-buffer-set 'right left)
  (e2wm:dp-two-update-history-list)))

(defun e2wm:dp-two-main-maximize-toggle-command ()
  (interactive)
  (wlf:toggle-maximize (e2wm:pst-get-wm) 'left))

(defvar e2wm:dp-two-minor-mode-map 
  (e2wm:define-keymap
   '(("prefix d" . e2wm:dp-two-double-column-command)
     ("prefix S" . e2wm:dp-two-sub-toggle-command)
     ("prefix -" . e2wm:dp-two-swap-buffers-command)
     ("prefix N" . e2wm:dp-two-right-history-down-command)
     ("prefix P" . e2wm:dp-two-right-history-up-command)
     ("prefix H" . e2wm:dp-two-history-toggle-command)
     ("prefix M" . e2wm:dp-two-main-maximize-toggle-command))
   e2wm:prefix-key))

;;; htwo / Horizontal split editing perspective
;;;--------------------------------------------------

(defvar e2wm:c-htwo-recipe
      '(| (:left-size-ratio 0.55)
          (| (:left-max-size 30)
             (- (:upper-size-ratio 0.7)
                files history)
             (- left right)) ; 継承してサボるためにleft,rightにする
          sub))

(defvar e2wm:c-htwo-winfo
      '((:name left )
        (:name right )
        (:name sub :buffer "*Help*" :default-hide t)
        (:name files :plugin files)
        (:name history :plugin history-list2)))

(e2wm:pst-class-register
  (make-e2wm:$pst-class
   :name   'htwo
   :extend 'two
   :title  "Horizontal Two"
   :init   'e2wm:dp-htwo-init))

(defun e2wm:dp-htwo-init ()
  (let* 
      ((htwo-wm 
        (wlf:no-layout 
         e2wm:c-htwo-recipe
         e2wm:c-htwo-winfo))
       (buf (or e2wm:prev-selected-buffer
                (e2wm:history-get-main-buffer))))

    (wlf:set-buffer htwo-wm 'left buf)
    (cond
     ((eq e2wm:c-two-right-default 'left)
      (wlf:set-buffer htwo-wm 'right buf))
     ((eq e2wm:c-two-right-default 'prev)
      (wlf:set-buffer htwo-wm 'right (e2wm:history-get-prev buf)))
     (t
      (wlf:set-buffer htwo-wm 'right (e2wm:history-get-prev buf))))

    htwo-wm))

(defun e2wm:dp-htwo ()
  (interactive)
  (e2wm:pst-change 'htwo))


;;; document / Document view perspective
;;;--------------------------------------------------

(defvar e2wm:c-doc-recipe
      '(- (:upper-size-ratio 0.75)
        (| left right)
        sub))

(defvar e2wm:c-doc-winfo
      '((:name left)
        (:name right)
        (:name sub :default-hide t)))

(e2wm:pst-class-register 
  (make-e2wm:$pst-class
   :name   'doc
   :extend 'base
   :init   'e2wm:dp-doc-init
   :title  "Document"
   :main   'left
   :start  'e2wm:dp-doc-start
   :update 'e2wm:dp-doc-update
   :switch 'e2wm:dp-doc-switch
   :popup  'e2wm:dp-doc-popup
   :leave  'e2wm:dp-doc-leave
   :keymap 'e2wm:dp-doc-minor-mode-map))

(defun e2wm:dp-doc-set-doc-buffer (buf)
  (e2wm:frame-param-set 'e2wm:dp-doc-buffer buf))

(defun e2wm:dp-doc-get-doc-buffer ()
  (e2wm:frame-param-get 'e2wm:dp-doc-buffer))

(defun e2wm:dp-doc-init ()
  (let* 
      ((doc-wm 
        (wlf:no-layout 
         e2wm:c-doc-recipe
         e2wm:c-doc-winfo))
       (buf (e2wm:dp-doc-get-doc-buffer)))

    (unless (and buf (buffer-live-p buf))
      (setq buf (or e2wm:prev-selected-buffer
                    (e2wm:history-get-main-buffer))))
    
    (wlf:set-buffer doc-wm 'left buf)
    doc-wm))

(defun e2wm:dp-doc-start (wm)
  (with-current-buffer (wlf:get-buffer wm 'left)
    (follow-mode 1)))

(defun e2wm:dp-doc-update (wm)
  (e2wm:message "#DP DOC update")
  (e2wm:$pst-class-super)
  ;;左右を同じにする
  (let ((leftbuf  (wlf:get-buffer wm 'left))
        (rightbuf (wlf:get-buffer wm 'right)))
    (unless (eql leftbuf rightbuf)
        (with-current-buffer leftbuf
          (follow-mode 1))
        (wlf:set-buffer wm 'right leftbuf))))

(defun e2wm:dp-doc-set-main-buffer (buf)
  (let ((wm (e2wm:pst-get-wm)))
    (with-current-buffer buf
      (follow-mode 1))
    (e2wm:pst-buffer-set 'left buf)
    (e2wm:pst-buffer-set 'right buf)))

(defun e2wm:dp-doc-switch (buf)
  ;;left,rightでswitch-to-bufferが起きたら、乗っ取って両方に表示する。
  (e2wm:message "#DP DOC switch : %s" buf)
  (let ((wm (e2wm:pst-get-wm))
        (curwin (selected-window)))
    (if (or (eql curwin (wlf:get-window wm 'left))
            (eql curwin (wlf:get-window wm 'right)))
        (progn 
          (e2wm:dp-doc-set-main-buffer buf)
          t)
      nil)))

(defun e2wm:dp-doc-popup (buf)
  (e2wm:message "#DP DOC popup : %s" buf)
  (let ((buf-name (buffer-name buf)))
    (cond
     ((or (e2wm:document-buffer-p buf)
          (e2wm:history-recordable-p buf))
      (e2wm:dp-doc-set-main-buffer buf)
      t)
     (t
      (e2wm:dp-doc-popup-sub buf)
      t))))

(defun e2wm:dp-doc-popup-sub (buf)
  (let ((wm (e2wm:pst-get-wm))
        (not-minibufp (= 0 (minibuffer-depth))))
    (e2wm:with-advice
     (e2wm:pst-buffer-set 'sub buf t not-minibufp))))

(defun e2wm:dp-doc-leave (wm)
  (let ((buf (get-buffer (wlf:get-buffer wm 'left))))
    (when (and buf (buffer-live-p buf))
      (unless (e2wm:history-recordable-p buf) ; ドキュメント的バッファだったら
        (e2wm:dp-doc-set-doc-buffer buf)      ; あとで再表示できるようにして、
        (setq e2wm:prev-selected-buffer nil))))   ; 次のパースペクティブは履歴から持ってきてもらう
  (loop for b in (buffer-list)
        do (with-current-buffer b
             (when follow-mode
               (follow-mode -1)))))

;; Commands / Keybindings

(defun e2wm:dp-doc ()
  (interactive)
  (e2wm:pst-change 'doc))

(defun e2wm:dp-doc-navi-main-command ()
  (interactive)
  (wlf:select (e2wm:pst-get-wm) 'left))
(defun e2wm:dp-doc-navi-sub-command ()
  (interactive)
  (wlf:select (e2wm:pst-get-wm) 'sub))
(defun e2wm:dp-doc-sub-toggle-command ()
  (interactive)
  (wlf:toggle (e2wm:pst-get-wm) 'sub)
  (e2wm:pst-update-windows))
(defun e2wm:dp-doc-main-maximize-toggle-command ()
  (interactive)
  (wlf:toggle-maximize (e2wm:pst-get-wm) 'left))

(defvar e2wm:dp-doc-minor-mode-map 
      (e2wm:define-keymap
       '(("prefix m" . e2wm:dp-doc-navi-main-command)
         ("prefix s" . e2wm:dp-doc-navi-sub-command)
         ("prefix S" . e2wm:dp-doc-sub-toggle-command)
         ("prefix M" . e2wm:dp-doc-main-maximize-toggle-command)
         ("prefix I" . info))
       e2wm:prefix-key))

;;; dashboard / dashboard buffers perspective
;;;--------------------------------------------------

(defvar e2wm:c-dashboard-plugins
  '(
    (open :plugin-args (:command eshell :buffer "*eshell*"))
    (open :plugin-args (:command doctor :buffer "*doctor*"))
    ))

(e2wm:pst-class-register
 (make-e2wm:$pst-class
     :name   'dashboard
     :extend 'base
     :title  "Dashboard"
     :main   'w-1-1
     :init   'e2wm:dp-dashboard-init
     :start  'e2wm:dp-dashboard-start
     :leave  'e2wm:dp-dashboard-leave))

(defun e2wm:dp-dashboard-init ()
  (let* ((size (e2wm:dp-array-calculate-size
                (length e2wm:c-dashboard-plugins)))
         (w (car size)) (h (cdr size))
         (recipe (e2wm:dp-array-make-recipe w h))
         (wparams (e2wm:dp-array-make-winfo w h))
         (wm (wlf:no-layout recipe wparams)))
    (e2wm:dp-dashboard-arrange-plugins wm)
    wm))

(defun e2wm:dp-dashboard-arrange-plugins (wm)
  (loop for winfo in (wlf:wset-winfo-list wm)
        with cnt = 0
        for opt = (wlf:window-options winfo)
        for plugin = (nth cnt e2wm:c-dashboard-plugins)
        do 
        (cond
         ((null plugin)
          (plist-put opt ':buffer e2wm:c-blank-buffer))
         ((symbolp plugin)
          (plist-put opt ':plugin plugin))
         ((consp plugin)
          (plist-put opt ':plugin (car plugin))
          (nconc opt (cdr plugin)))
         (t
          (plist-put opt ':buffer e2wm:c-blank-buffer)))
        (incf cnt))
  wm)

(defun e2wm:dp-dashboard-start (wm)
  (e2wm:dp-dashboard-update-summary))

(defun e2wm:dp-dashboard-leave (wm)
  (unless (e2wm:history-recordable-p e2wm:prev-selected-buffer)
    (setq e2wm:prev-selected-buffer nil)
    (setq prev-selected-buffer nil)))   ; 次のパースペクティブは履歴から持ってきてもらう

(defun e2wm:dp-dashboard-update-summary ()
  (let* ((bufname " *WM:EmacsStat*")
         (buf (get-buffer bufname))
         (wm (e2wm:pst-get-wm)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create bufname))
      (with-current-buffer buf
        (setq buffer-read-only t)
        (buffer-disable-undo buf)))
    (with-current-buffer buf
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (erase-buffer)
            (goto-char (point-min))
            (e2wm:dp-dashboard-insert-summary-info))
        (setq buffer-read-only t)))
    (wlf:set-buffer wm 'summary buf)))

(defun e2wm:dp-dashboard-insert-summary-info ()
  (multiple-value-bind
      (conses syms miscs used-string-chars 
              used-vector-slots floats intervals strings)
      (garbage-collect)
    (let* ((used-conses (car conses))
           (free-conses (cdr conses))
           (used-miscs (car miscs))
           (free-miscs (cdr miscs))
           (used-syms (car syms))
           (free-syms (cdr syms))
           (used-floats (car floats))
           (free-floats (cdr floats))
           (used-strings (car strings))
           (free-strings (cdr strings))
           (used-intervals (car intervals))
           (free-intervals (cdr intervals))
           (buf-num (length (buffer-list)))
           (pure-mem (e2wm:format-byte-unit pure-bytes-used))
           (gccons (e2wm:format-byte-unit gc-cons-threshold))
           (mem-limit (e2wm:format-byte-unit (* 1024 (memory-limit)))))
      (flet
          ((percent (used free)
                    (let ((u (* 1.0 used)))
                      (format "%.2f" (* 100.0 (/ u (+ u free)))))))
        (insert 
         (e2wm:rt-format "Emacs stat:\n"))
        (insert
         (e2wm:rt-format "  Conses: %s (%s%%) /  Syms: %s (%s%%) / Miscs: %s (%s%%) / String Chars %s\n"
                        (e2wm:num used-conses) (percent used-conses free-conses)
                        (e2wm:num used-syms) (percent used-syms free-syms)
                        (e2wm:num used-miscs) (percent used-miscs free-miscs)
                        (e2wm:num used-string-chars)))
        (insert
         (e2wm:rt-format "  Floats: %s (%s%%) /  Strings: %s (%s%%) / Intervals: %s (%s%%) / Vectors %s\n"
                        (e2wm:num used-floats) (percent used-floats free-floats)
                        (e2wm:num used-strings) (percent used-strings free-strings)
                        (e2wm:num used-intervals) (percent used-intervals free-intervals)
                        (e2wm:num used-vector-slots)))
         (insert
          (e2wm:rt-format "  Buffers: %s  /  Memory Limit: %s  /  Pure: %s"
                         buf-num mem-limit pure-mem))))))

;; Commands / Keybindings

(defun e2wm:dp-dashboard ()
  (interactive)
  (e2wm:pst-change 'dashboard))

;;; array / arrange buffers perspective
;;;--------------------------------------------------

(defvar e2wm:c-array-font-decrease 3) ; フォントのを小さくする相対サイズ
(defvar e2wm:c-array-max-rows 4)  ; 並べる横最大数
(defvar e2wm:c-array-max-cols 5)  ; 並べる縦最大数

(defvar e2wm:c-array-more-buffers-pred
  (lambda (b)
    (let ((bn (buffer-name b)))
      (and 
       (not  ; 表示しないもの
        (memq (buffer-local-value 'major-mode b)  
              '(dired-mode)))
       (or
        (e2wm:document-buffer-p b) ; ドキュメントは表示する
        (not (string-match "^ ?\\*" bn)) ; 内部バッファは表示しないが、
        (string-match ; 以下のものは表示する
         "\\*\\(scratch\\|Messages\\)\\*" 
         bn))))))
      
(defun e2wm:dp-array-make-recipe (cols rows)
  ;; cols x rows の recipe を作る
  (let* ((sz-summary 0.12)
         (sz-array (- 1.0 sz-summary)))
    (decf cols) (decf rows)
    (labels
        ((loop-rows
          (cols rows)
          (loop for i from rows downto 1
                with ret = nil
                if (< i rows)
                do (setq ret 
                         (list 
                          '- (list :upper-size-ratio 
                                   (* sz-array (/ 1.0 (- rows (- i 1.5)))))
                          (loop-cols cols i) ret))
                else
                do (setq ret 
                         (list '- (loop-cols cols i)
                               (loop-cols cols (1+ i))))
                finally return ret))
         (loop-cols
          (cols y)
          (loop for i from cols downto 1
                with ret = nil
                if (< i cols)
                do (setq ret (list 
                              '| (list :left-size-ratio
                                       (/ 1.0 (- cols (- i 2))))
                              (mk i y) ret))
                else
                do (setq ret (list '| (mk i y) (mk (1+ i) y)))
                finally return ret))
         (mk (x y) (intern (format "w-%i-%i" x y))))
      (let ((ar (cond
                 ((and (eql cols 0) (eql rows 0))
                  'w-1-1)
                 ((< rows 1)
                  (loop-cols 1 1))
                 (t
                  (loop-rows cols rows)))))
        (list '- (list ':upper-size-ratio sz-array) ar 'summary)))))

(defun e2wm:dp-array-make-winfo (cols rows)
  ;; cols x rows の winfo を作る
  (labels
      ((loop-rows (cols rows)
                  (loop for i from 1 to rows
                        with ret = nil
                        do (setq ret (nconc ret (loop-cols cols i)))
                        finally return ret))
       (loop-cols (cols y)
                  (loop for i from 1 to cols
                        with ret = nil
                        do (setq ret (nconc ret (list (mk i y))))
                        finally return ret))
       (mk (x y) (list ':name (intern (format "w-%i-%i" x y)))))
    (let ((ar (loop-rows cols rows)))
      (add-to-list 'ar '(:name summary) t))))

(defun e2wm:dp-array-calculate-size (num)
  ;;num個のサイズが入る縦横幅を計算する
  ;;なるべく縦横の数が離れすぎないような数を探す
  (labels
      ((loop-rows (cols rows)
                  (loop for i from 1 to rows
                        for ret = (loop-cols i i cols)
                        if ret
                        return ret
                        finally return (cons cols rows)))
       (loop-cols (cols y mx)
                  (loop for i from y to (min (1+ y) mx)
                        if (<= num (* i y))
                        return (cons i y)
                        finally return nil)))
    (loop-rows e2wm:c-array-max-cols 
               e2wm:c-array-max-rows)))

(defun e2wm:dp-array-make-wm (buffers)
  (let* ((size (e2wm:dp-array-calculate-size (length buffers)))
         (w (car size)) (h (cdr size))
         (recipe (e2wm:dp-array-make-recipe w h))
         (wparams (e2wm:dp-array-make-winfo w h))
         (wm (wlf:no-layout recipe wparams)))
    (e2wm:dp-array-arrange-buffers wm buffers)
    wm))

(e2wm:pst-class-register 
 (make-e2wm:$pst-class
     :name   'array
     :extend 'base
     :title  "Buffer Array"
     :main   'w-1-1
     :init   'e2wm:dp-array-init
     :popup  'e2wm:dp-array-popup
     :start  'e2wm:dp-array-start
     :leave  'e2wm:dp-array-leave))

(defvar e2wm:dp-array-buffers-function
  'e2wm:dp-array-get-recordable-buffers) ; この関数を切り替える

(defun e2wm:dp-array-init ()
  (let* 
      ((array-wm (e2wm:dp-array-make-wm 
                  (funcall e2wm:dp-array-buffers-function))))
    array-wm))

(defvar e2wm:dp-array-backup-globalmap nil)

(defun e2wm:dp-array-start (wm)
  (e2wm:message "#ARRAY START")
  (setq e2wm:dp-array-backup-globalmap global-map)
  (use-global-map e2wm:dp-array-minor-mode-map) ; 強引
  (setq overriding-terminal-local-map e2wm:dp-array-minor-mode-map)
  (e2wm:dp-array-decrease-fontsize)
  (e2wm:dp-array-update-summary))

(defun e2wm:dp-array-leave (wm)
  (e2wm:message "#ARRAY LEAVE")
  (use-global-map e2wm:dp-array-backup-globalmap)
  (setq overriding-terminal-local-map nil)
  (when e2wm:dp-array-overlay-focus
    (delete-overlay e2wm:dp-array-overlay-focus))
  (e2wm:dp-array-increase-fontsize))

(defun e2wm:dp-array-arrange-buffers (wm buffers)
  (loop for winfo in (wlf:wset-winfo-list wm)
        with cnt = 0
        for opt = (wlf:window-options winfo)
        do (plist-put 
            opt ':buffer
            (e2wm:aif (nth cnt buffers) 
                it e2wm:c-blank-buffer))
        (incf cnt))
  wm)

(defun e2wm:dp-array-get-recordable-buffers ()
  ;;履歴に記録しそうなもの一覧
  (let ((ret 
         (append 
          (reverse (e2wm:history-get))
          (copy-list (e2wm:history-get-backup))
          )))
    (loop for b in (buffer-list)
          if (and (e2wm:history-recordable-p b)
                  (not (member b ret)))
          do (push b ret))
    (nreverse ret)))

(defun e2wm:dp-array-get-more-buffers ()
  ;;表示して意味がありそうなもの
  (let ((ret 
         (append 
          (reverse (e2wm:history-get))
          (copy-list (e2wm:history-get-backup))
          )))
    (loop for b in (buffer-list)
          if (and (funcall e2wm:c-array-more-buffers-pred b)
                  (not (member b ret)))
          do (push b ret))
    (nreverse ret)))

(defun e2wm:dp-array-popup (buf)
  (e2wm:message "#DP ARRAY popup : %s" buf)
  (let ((wm (e2wm:pst-get-wm)))
    (e2wm:with-advice
     (wlf:set-buffer wm 'summary buf)))
  t)

(defun e2wm:dp-array-decrease-fontsize ()
  (when (fboundp 'text-scale-decrease)
    (loop for b in (buffer-list)
          if (not (minibufferp b))
          do (with-current-buffer b
               (text-scale-decrease e2wm:c-array-font-decrease)))))

(defun e2wm:dp-array-increase-fontsize ()
  (when (fboundp 'text-scale-increase)
    (loop for b in (buffer-list)
          if (not (minibufferp b))
          do (with-current-buffer b
               (text-scale-increase e2wm:c-array-font-decrease)))))

(defun e2wm:dp-array-update-summary ()
  (let* ((bufname " *WM:ArraySummary*")
         (buf (get-buffer bufname))
         (wm (e2wm:pst-get-wm))
         (win (wlf:get-window wm 'summary))
         (selected-buf (window-buffer (selected-window))))
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create bufname))
      (with-current-buffer buf
        (setq buffer-read-only t)
        (buffer-disable-undo buf)))
    (with-current-buffer buf
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (erase-buffer)
            (goto-char (point-min))
            (unless (eql selected-buf buf)
              (e2wm:dp-array-insert-summary-info
               selected-buf))
        (setq buffer-read-only t))))
    (wlf:set-buffer wm 'summary buf))
  (e2wm:dp-array-hilite-focus))

(defun e2wm:dp-array-insert-summary-info (selected-buf)
  (cond
   ((buffer-local-value 'buffer-file-name selected-buf)
    (let* ((f (buffer-local-value 'buffer-file-name selected-buf))
           (filename (file-name-nondirectory f))
           (vc (e2wm:aif (buffer-local-value 'vc-mode selected-buf)
                   (substring it 0) "None"))
           (dir (file-name-directory f))
           (mode (file-modes f)) 
           (attr (file-attributes f))
           (modified-time (nth 5 attr))
           (size (nth 7 attr))
           (strsize (e2wm:format-byte-unit size)))
      (insert
       (e2wm:rt-format "File Name: %s  (Path: %s)\n"
               (cons  filename 'e2wm:face-title) dir)
       (e2wm:rt-format "Mode: %s  /  Modified Time: %s\n"
               (format "%o" mode) (e2wm:strtime modified-time))
       (e2wm:rt-format "File Size: %s  /  Lines: %s  /  Version Control: %s"
               strsize 
               (int-to-string (with-current-buffer selected-buf 
                 (count-lines (point-min) (point-max))))
               vc))))
   (t
    (insert
     (e2wm:rt-format 
      "Buffer Name: %s\nMajor Mode: %s"
      (substring-no-properties (buffer-name selected-buf))
      (format "%s" (buffer-local-value
                    'major-mode selected-buf)))))))

(defvar e2wm:dp-array-overlay-focus nil "[internal]")

(defun e2wm:dp-array-hilite-focus ()
  (when e2wm:dp-array-overlay-focus
    (delete-overlay e2wm:dp-array-overlay-focus))
  (setq e2wm:dp-array-overlay-focus 
        (make-overlay (point-min) (point-max)))
  (overlay-put e2wm:dp-array-overlay-focus 'face 'highlight))

;; Commands / Keybindings

(defun e2wm:dp-array ()
  (interactive)
  (e2wm:pst-change 'array))

(defun e2wm:dp-array-move-left-command ()
  (interactive)
  (e2wm:not-minibuffer
   (windmove-left)
   (e2wm:dp-array-update-summary)))
(defun e2wm:dp-array-move-right-command ()
  (interactive)
  (e2wm:not-minibuffer
   (windmove-right)
   (e2wm:dp-array-update-summary)))
(defun e2wm:dp-array-move-up-command ()
  (interactive)
  (e2wm:not-minibuffer
   (windmove-up)
   (e2wm:dp-array-update-summary)))
(defun e2wm:dp-array-move-down-command ()
  (interactive)
  (e2wm:not-minibuffer
   (let ((cwin (selected-window))
         (bwin (wlf:get-window (e2wm:pst-get-wm) 'summary)))
     (windmove-down)
     (when (eql (selected-window) bwin)
       (select-window cwin)))
   (e2wm:dp-array-update-summary)))
(defun e2wm:dp-array-goto-prev-pst-command ()
  (interactive)
  (e2wm:not-minibuffer
   (e2wm:pst-change-prev)))
(defun e2wm:dp-array-toggle-more-buffers-command ()
  (interactive)
  (e2wm:not-minibuffer
   (setq e2wm:dp-array-buffers-function
         (if (eq e2wm:dp-array-buffers-function
                 'e2wm:dp-array-get-recordable-buffers)
             'e2wm:dp-array-get-more-buffers
           'e2wm:dp-array-get-recordable-buffers))
   (e2wm:pst-change 'array)))
(defun e2wm:dp-array-cancel-command ()
  (interactive)
  (e2wm:not-minibuffer
   (wlf:select (e2wm:pst-get-wm) 'w-1-1)
   (e2wm:pst-change-prev)))

(defvar e2wm:dp-array-minor-mode-map 
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap)
    (e2wm:add-keymap keymap
   '(("<SPC>"  . e2wm:dp-array-toggle-more-buffers-command)
     ;;cursor
     ([left]  . e2wm:dp-array-move-left-command)
     ([right] . e2wm:dp-array-move-right-command)
     ([up]    . e2wm:dp-array-move-up-command)
     ([down]  . e2wm:dp-array-move-down-command)
     ;;emacs
     ("f"   . e2wm:dp-array-move-right-command)
     ("b"   . e2wm:dp-array-move-left-command)
     ("p"   . e2wm:dp-array-move-up-command)
     ("n"   . e2wm:dp-array-move-down-command)
     ("C-f" . e2wm:dp-array-move-right-command)
     ("C-b" . e2wm:dp-array-move-left-command)
     ("C-p" . e2wm:dp-array-move-up-command)
     ("C-n" . e2wm:dp-array-move-down-command)
     ;;vi
     ("h"   . e2wm:dp-array-move-left-command)
     ("l"   . e2wm:dp-array-move-right-command)
     ("k"   . e2wm:dp-array-move-up-command)
     ("j"   . e2wm:dp-array-move-down-command)
     ;;choose
     ("q"   . e2wm:dp-array-cancel-command)
     ("C-m" . e2wm:dp-array-goto-prev-pst-command)
     ("C-g" . e2wm:dp-array-cancel-command)
     ))
    keymap))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Setup

(e2wm:add-keymap
 e2wm:pst-minor-mode-keymap
 '(("prefix 1" . e2wm:dp-code)
   ("prefix 2" . e2wm:dp-two)
   ("prefix 3" . e2wm:dp-doc)
   ("prefix 4" . e2wm:dp-array)
   ("prefix 5" . e2wm:dp-dashboard))
 e2wm:prefix-key)

(defun e2wm:history-add-loaded-buffers ()
  "Put all recordable buffers in the history list."
  (interactive)
  (loop for b in (buffer-list)
        for bo = (get-buffer b)
        if (e2wm:history-recordable-p bo)
        do (e2wm:history-add bo)))

(defvar e2wm:pre-start-hook nil "")
(defvar e2wm:post-start-hook nil "")

(defun e2wm:start-management (&optional pstset force-restart)
  "e2wm window management for the current frame.

To force restart use the universal prefix argument (C-u) or
specify non-nil for FORCE-STOP when calling as a lisp function."
  (interactive)

  (when (or force-restart current-prefix-arg)
    (e2wm:stop-management t)
    (message "Restarting e2wm..."))

  (cond
   (e2wm:pst-minor-mode
    (message "E2wm has already started."))
   (t
    (run-hooks 'e2wm:pre-start-hook)

    (e2wm:frame-param-set
     'e2wm-save-window-configuration
     (current-window-configuration))

    (e2wm:history-add-loaded-buffers)
    (e2wm:history-save-backup nil)

    (e2wm:pst-minor-mode 1)
    (ad-activate-regexp "^e2wm:ad-debug" t) ; debug

    (if pstset
        (e2wm:pstset-define pstset)
      (e2wm:pstset-defaults)) ; use all registered perspectives
    (e2wm:pst-set-prev-pst nil)
    ;; show the first perspective in the perspective set
    (e2wm:pst-change (car (e2wm:pstset-get-current-pstset)))
    (e2wm:menu-define)

    (run-hooks 'e2wm:post-start-hook)
    (message "E2wm is started."))))

(defvar e2wm:post-stop-hook nil "")

(defun e2wm:stop-management (&optional force-stop)
  "Stop e2wm window management for the current frame.

To force stop, use the universal prefix argument (C-u) or
specify non-nil for FORCE-STOP when calling as a lisp function."
  (interactive)
  (setq force-stop (or current-prefix-arg force-stop))
  (when (or force-stop (e2wm:managed-p))
    (e2wm:pst-finish)
    (e2wm:pst-minor-mode -1)
    (e2wm:pst-set-prev-pst nil)

    (ad-deactivate-regexp "^e2wm:ad-debug") ; debug

    (e2wm:aif (e2wm:frame-param-get
               'e2wm-save-window-configuration)
        (set-window-configuration it))
    (run-hooks 'e2wm:post-stop-hook))
  (when force-stop
    (message "E2wm is stopped forcefully.")))

;; for dev
;; (progn (setq e2wm:debug t) (toggle-debug-on-error))
;; (progn (kill-buffer (get-buffer-create "*e2wm:debug*")) (eval-current-buffer) (e2wm:start-management))
;; (e2wm:stop-management)

(provide 'e2wm)
;;; e2wm.el ends here
