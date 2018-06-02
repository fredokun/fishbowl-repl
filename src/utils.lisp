
(in-package #:fredokun-utilities)


(declaim (inline []))
(defun [] (table key &optional (default nil defaultp))
  (let ((pair (assoc key table :test #'equal)))
    (if pair
        (cdr pair)
        (if defaultp
            default
            (error "Could not find key ~a in dict ~a" key table)))))

(defun []-contains (table key)
  (let ((pair (assoc key table :test #'equal)))
    pair))

(defun getpid ()
  "Return the pid of the current process"
  #+clasp(core:getpid)
  #+sbcl(sb-posix:getpid))


(defun function-lambda-list (function)
  "Return the lambda-list of a function - there is no standard way of doing this"
  #+clasp(core:function-lambda-list function)
  #+sbcl(sb-introspect:function-lambda-list function))


(defun backtrace-as-string ()
  "Returns the backtrace as a string for logging crashes"
  #+clasp(with-output-to-string (*standard-output*) (core::clasp-backtrace))
  #+sbcl "Generate a backtrace for sbcl")

#|

# CommonTypes: Utilities #

|#

;; To activate the inline examples
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *example-enabled* t) ;; nil in production / t for self-testing

  (defparameter *example-equal-predicate* #'equal)

  (defparameter *example-with-echo* nil)
  
  )


(defmacro example (expr arrow expected &key (warn-only nil))
  "Show an evaluation example, useful for documentation and lightweight testing.

   (example `EXPR` => `EXPECTED`) evaluates `EXPR` and compare, wrt. `EQUIV`
 (EQUAL by default) to `EXPECTED` and raise an error if inequal.

  Set `WARN-ONLY` to T for warning instead of error.
"
  (if (not *example-enabled*)
      (progn
        (when *example-with-echo*
          (format t "------------------~%")
          (format t "Example:~%~A~%=?= ~A~%" (format nil "~A" expr) expected)
          (format t "  ===> SKIP~%"))
        (values));; synonymous of nil if disabled
      ;; when enabled
      (let ((result-var (gensym "result-"))
            (expected-var (gensym "expected-"))
            (err-fun-var (gensym "err-fun-"))
            (expr-str (format nil "~A" expr)))
        `(progn
           (when *example-with-echo*
             (format t "------------------~%")
             (format t "Example:~%~A~%=?= ~A~%" ,expr-str ,expected))
           (let ((,err-fun-var (if ,warn-only #'warn #'error))
                 (,result-var ,expr)
                 (,expected-var ,expected))
             (if (not (equal (symbol-name (quote ,arrow)) "=>"))
                 (error "Missing arrow '=>' in example expression"))
             (if (funcall *example-equal-predicate* ,result-var ,expected-var)
                 (progn (if *example-with-echo*
                            (format t "  ===> PASS~%"))
                        t)
                 (funcall ,err-fun-var "Failed example:~%  Expression: ~S~%  ==> expected: ~A~%  ==> evaluated: ~A~%"
                          ,expr-str ,expected-var ,result-var)))))))


(defmacro example-progn (&body body)
  "The toplevel forms of BODY are evaluated only if examples are enabled"
  (if *example-enabled*
      `(progn ,@body)
      (values)))

(defparameter *log-enabled* nil)
(defparameter *log-level* nil)
(defparameter *log-out-stream* nil)

(defparameter *log-file* nil)
(defparameter *log-lock* nil)

;;; Comment out the following eval-when if you want logging fully disabled
#+(or)
(eval-when (:execute :load-toplevel :compile-toplevel)
  (format t "Turning on cl-jupyter logging~%")
  (push :cl-jupyter-log *features*))

#+cl-jupyter-log
(eval-when (:execute :load-toplevel)
  (setf *log-enabled* t)
  (setf *log-level* 2)
  (let* ((log-file-name (make-pathname :name (format nil "cl-jupyter-~a" (getpid))
                                       :type "log"))
         (log-path-name (cond
                          ((probe-file "/home/app/logs/")
                           (merge-pathnames log-file-name #P"/home/app/logs/"))
                          (t (merge-pathnames log-file-name #P"/tmp/")))))
    (setf *log-file* (cl:open log-path-name
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist :create))
    (format *log-file* "Log started up~%")
    (setf *log-lock* (bordeaux-threads:make-lock "cl-jupyter-log"))
    (format *log-file* "About to start logging cl-jupyter~%")
    (defun always-log (fmt &rest args)
      (let ((msg (apply #'format nil fmt args)))
        (unwind-protect
             (progn
               (bordeaux-threads:acquire-lock *log-lock* t)
               (princ msg *log-file*)
               (finish-output *log-file*))
          (bordeaux-threads:release-lock *log-lock*))))
    (format *log-file* "===================== new run =======================~%")))

#+cl-jupyter-log
(defmacro logg (level fmt &rest args)
  "Log the passed ARGS using the format string FMT and its
 arguments ARGS."
  (if (or (not *log-enabled*)
          (< level *log-level*))
      (values);; disabled
      ;; when enabled
      `(progn (always-log ,fmt ,@args))))

#-cl-jupyter-log
(defmacro logg (level fmt &rest args)
  nil)

(defmacro vbinds (binders expr &body body)
  "An abbreviation for MULTIPLE-VALUE-BIND."
  (labels ((replace-underscores (bs &optional (result nil) (fresh-vars nil) (replaced nil))
             (if (null bs)
                 (let ((nresult (nreverse result))
                       (nfresh (nreverse fresh-vars)))
                   (values replaced nresult nfresh))
                 (if (equal (symbol-name (car bs)) "_")
                     (let ((fresh-var (gensym "underscore-")))
                       (replace-underscores (cdr bs) (cons fresh-var result) (cons fresh-var fresh-vars) t))
                     (replace-underscores (cdr bs) (cons (car bs) result) fresh-vars replaced)))))
    (multiple-value-bind (has-underscore nbinders fresh-vars) (replace-underscores binders)
      (if has-underscore
          `(multiple-value-bind ,nbinders ,expr
             (declare (ignore ,@fresh-vars))
             ,@body)
          `(multiple-value-bind ,binders ,expr ,@body)))))

(example (vbinds (a _ b) (values 1 2 3)
           (cons a b))
         => '(1 . 3)) ;; without a warning

(example (vbinds (a _ b _) (values 1 2 3 4)
           (cons a b))
         => '(1 . 3)) ;; without a warning


(defun afetch (comp alist &key (test #'eql))
  (let ((binding (assoc comp alist :test test)))
    (if binding
        (cdr binding)
        (error "No such key: ~A" comp))))

(defmacro while (condition &body body)
  (let ((eval-cond-var (gensym "eval-cond-"))
        (body-val-var (gensym "body-val-")))
    `(flet ((,eval-cond-var () ,`,condition))
       (do ((,body-val-var nil (progn ,@body)))
           ((not (,eval-cond-var))
            ,body-val-var)))))

(example (let ((count 0))
           (while (< count 10)
             ;;(format t "~A " count)
             (incf count)
             count))
         => 10)

(defun read-file-lines (filename)
  (with-open-file (input filename)
    (loop
       for line = (read-line input nil 'eof)
       until (eq line 'eof)
       collect line)))

(defun read-binary-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun read-string-file (filename)
  (with-open-file (stream filename)
    (let ((str (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer str) (read-sequence str stream))
      str)))


;; Taken from Rob Warnock's post "How to programmatically exit?"
(defun quit (&optional code)
      ;; This group from "clocc-port/ext.lisp"
      #+allegro (excl:exit code)
      #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
      #+cmu (ext:quit code)
      #+cormanlisp (win32:exitprocess code)
      #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
      #+lispworks (lw:quit :status code)
      #+lucid (lcl:quit code)
      #+sbcl (sb-ext:exit :code code)
      ;; This group from Maxima
      #+kcl (lisp::bye)                         ; XXX Does this take an arg?
      #+scl (ext:quit code)                     ; XXX Pretty sure this *does*.
      #+(or openmcl mcl) (ccl::quit)
      #+abcl (cl-user::quit)
      #+(or ecl clasp) (si:quit)
      ;; This group from <hebi...@math.uni.wroc.pl>
      #+poplog (poplog::bye)                    ; XXX Does this take an arg?
      #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
            kcl scl openmcl mcl abcl ecl clasp)
      (error 'not-implemented :proc (list 'quit code))) 
