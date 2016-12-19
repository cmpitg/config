#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload '("quickproject"
                "iterate"        ;; Looping
                "alexandria"     ;; Various utilities
                "cl-unicode"     ;; Unicode
                "trivial-utf-8"  ;; Unicode UTF-8
                ;; "optima"         ;; Pattern matching
                "trivia"         ;; Optima-compatible pattern matching but
                                 ;; better
                "ironclad"       ;; Cryptography
                "stmx"           ;; STM implementation
                "inferior-shell" ;; One of the best shell scripting libraries
                "qlot"           ;; Local, per-project Quicklisp
                "cl-cwd"         ;; Current working directory
                "shelly"         ;; Shelly
                "split-sequence" ;; Helpers for seq splitting
                "lisp-unit"      ;; Unit testing
                "cl-ppcre"       ;; Regular expression
                "lisp-unit"      ;; Unit testing

                "inlined-generic-function"
                "quicklisp-slime-helper"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf lisp-unit:*print-failures* t
      lisp-unit:*print-errors*   t)

(defpackage #:poignant
  (:use #:cl #:iterate)
  (:export #:md5-digest
           #:sha1-digest
           #:newline
           #:println
           #:str
           #:slurp
           #:load-slime
           #:alias-package
           #:!cmd
           #:sethash
           #:take-while
           #:string-starts-with?
           #:string-ends-with?
           #:drop-while
           #:all-function-symbols
           #:tty?
           #:->
           #:->>))

(in-package #:poignant)

(defun tty? (&optional (stream *standard-input*))
  "Checks if current stdin is a terminal."
  (interactive-stream-p stream))

(defun md5-digest (str)
  "Calculates MD5 digest of a string.  Require: ironclad."
  (declare (string str))
  (the string
       (ironclad:byte-array-to-hex-string
        (ironclad:digest-sequence
         :md5
         (ironclad:ascii-string-to-byte-array str)))))

(defun sha1-digest (str)
  "Calculates SHA1 digest of a string.  Require: ironclad."
  (declare (string str))
  (the string
       (ironclad:byte-array-to-hex-string
        (ironclad:digest-sequence
         :sha1
         (ironclad:ascii-string-to-byte-array str)))))

(defun newline (&optional (stream t))
  "Simply prints a newline."
  (declare ((or boolean stream) stream))
  (princ #\Newline stream))

(defun println (&rest obj)
  "Wraps around `(format t ...)', followed by a newline."
  (prog1
      (apply #'format t obj)
    (princ #\Newline)))

(defun str (&rest args)
  "Convenient way to build string by converting all arguments and
concatenating them."
  (format nil "~{~A~}" args))

(defun load-slime (&optional (port 4005))
  "Loads all necessities for Slime."
  (ql:quickload "swank")
  (swank:create-server :style :spawn :dont-close t :port port))

(defun alias-package (package alias)
  "Adds an alias for a package.  The list of aliases could be retrieved with
`package-nicknames'."
  (declare ((or string package symbol) package)
           ((or string symbol) alias))
  (let ((package (if (stringp package)
                     (find-package package)
                   package))
        (alias (string-upcase (if (stringp alias)
                                  alias
                                (symbol-name alias)))))
    (rename-package package
                    (package-name package)
                    (adjoin alias (package-nicknames package)
                            :test #'string=))))

(defun !cmd (cmd &key (print-command nil)
                   (output t)
                   (error-output t)
                   (force-shell t))
  "Runs command by calling `uiou:run-program'."
  (declare (string cmd)
           (boolean print-command force-shell)
           ((or boolean stream) output error-output))
  (when print-command
    (format output cmd)
    (format output "~%"))
  (uiop:run-program cmd :output output
                    :error-output error-output
                    :force-shell force-shell))

(defun sethash (key hash value &rest args)
  "Conveniently combining `setf' and `gethash'.

`\(setf \(gethash o hash\) obj\)' ⬄ `\(sethash o hash obj\)'
`\(setf \(gethash o hash\) obj
      \(gethash a hash\) abj\)'
⬄
`\(sethash o hash obj
         a hash abj\)'
"
  (declare (hash-table hash))
  (format t "~A~%" args)
  (unless (zerop (mod (length args) 3))
    (error "number of arguments must be divisible by 3."))

  (setf (gethash key hash) value)

  (unless (null args)
    (apply #'sethash (first args) (second args) (third args) (subseq args 3))))

(defun take-while (fn xs)
  "Takes each `item' of `xs' from the beginning and builds a list until
`\(funcall fn item\)' returns `nil'.

E.g.

  \(take-while #'oddp '\(\)\)          ;; ⇨ '\(\)
  \(take-while #'oddp '\(2 3 4\)\)     ;; ⇨ '\(\)
  \(take-while #'evenp '\(2 2 4\)\)    ;; ⇨ '\(2 2 4\)
  \(take-while #'evenp '\(2 2 1 3\)\)  ;; ⇨ '\(2 2\)"
  (declare (function fn) (sequence xs))
  (the list (iterate (for x in xs)
                     (while (funcall fn x))
                     (collect x))))

(defun string-starts-with? (str sub)
  "Determines if string `str' starts with `sub'."
  (declare (string str sub))
  (the boolean (let ((pos (search sub str)))
                 (and pos (= 0 pos)))))

(defun string-ends-with? (str sub)
  "Determines if string `str' ends with `sub'."
  (declare (string str sub))
  (the boolean (let ((pos (mismatch sub str :from-end t)))
                 ;; partial match: pos is nil
                 ;; full match:    pos is 0
                 (or (not pos) (= 0 pos)))))

(defun drop-while (fn xs)
  "Starting from the first item of `xs' that `\(funcall fn item\)' returns
`nil', builds a list with the rest of `xs'.

E.g.

  \(drop-while #'oddp '\(\)\)            ;; ⇨ '\(\)
  \(drop-while #'oddp '\(1 1 5 7\)\)     ;; ⇨ '\(\)
  \(drop-while #'oddp '\(1 2 5 2 3 4\)\) ;; ⇨ '\(2 3 4\)
  \(drop-while #'oddp '\(2 3 4\)\)       ;; ⇨ '\(2 3 4\)"
  (declare (function fn) (sequence xs))
  (the list (labels ((helper (xs)
                       (cond ((null xs)
                              (list))
                             ((not (funcall fn (first xs)))
                              xs)
                             (t
                              (helper (rest xs))))))
              (helper xs))))

(defun all-function-symbols (package-name &key (excludes-imports? t))
  "Retrieves all function symbols from a package."
  (declare ((or package string symbol) package-name)
           (boolean excludes-imports?))
  (the list
       (let ((lst (list))
             (package (find-package (typecase package-name
                                      (string (string-upcase package-name))
                                      (t      package-name)))))
         (cond (package
                (do-all-symbols (symb package)
                  (when (and (fboundp symb)
                             (or (null excludes-imports?)
                                 (and excludes-imports?
                                      (eql (symbol-package symb) package))))
                    (push symb lst)))
                lst)
               (t
                (error "~S does not designate a package" package-name))))))

;;;
;; (defpackage #:foobar
;;   (:use :cl)
;;   (:export #:public-func))
;;
;; (in-package #:foobar)
;; (defun private-func () (format t "A private function 2~%"))
;; (defun public-func () (format t "A public function~%"))
;;
;; (in-package :cl-user)
;; (all-function-symbols "foobar")
;; (all-function-symbols 'foobar)
;; (length (all-function-symbols 'foobar :excludes-imports? nil))
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slurp (thing)
  "Slurps a text file or a text stream."
  (declare ((or string pathname stream) thing))
  (labels ((do-slurp (path)
             (with-open-file (in path :element-type '(unsigned-byte 8))
               (trivial-utf-8:read-utf-8-string in :stop-at-eof t))))
    (ctypecase thing
      (string (do-slurp thing))
      (pathname (do-slurp (namestring thing)))
      (stream (let ((seq (make-array (file-length thing)
                                     :element-type 'character
                                     :fill-pointer t)))
                (setf (fill-pointer seq) (read-sequence seq thing))
                seq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro -> (initial-form &rest forms)
  "Naive implementation of pipe first macro.  Inspired by Clojure's so-called
'thread-first' macro.

I.e.

\(-> someform
  do-something
  \(do-something-else argument\)\)

is equivalent to

\(do-something-else \(do-something someform\) argument\)
"
  (let ((output-form initial-form)
        (remaining-forms forms))
    (loop while remaining-forms do
         (let ((current-form (car remaining-forms)))
           (if (listp current-form)
               (setf output-form (cons (car current-form)
                                       (cons output-form (cdr current-form))))
               (setf output-form (list current-form output-form))))
         (setf remaining-forms (cdr remaining-forms)))
    output-form))

(defmacro ->> (initial-form &rest forms)
  "Naive implementation of pipe last macro.  Inspired by Clojure's so-called
'thread-last' macro.

I.e.

\(->> someform
  do-something
  \(do-something-else argument\)\)

is equivalent to

\(do-something-else argument \(do-something someform\)\)
"
  (let ((output-form initial-form)
        (remaining-forms forms))
    (loop while remaining-forms do
         (let ((current-form (car remaining-forms)))
           (if (listp current-form)
               (setf output-form (cons (car current-form)
                                       (append (cdr current-form) (list output-form))))
               (setf output-form (list current-form output-form))))
         (setf remaining-forms (cdr remaining-forms)))
    output-form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:cl-user)

(use-package :poignant)
