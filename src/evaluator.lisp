(in-package #:cl-jupyter)

#|

# Evaluator #

The evaluator is where the "interesting stuff" takes place :
 user expressions are evaluated here.

The history of evaluations is also saved by the evaluator.

|#


(defclass evaluator ()
  ((kernel :initarg :kernel :reader evaluator-kernel)
   (history-in :initform (make-array 64 :fill-pointer 0 :adjustable t)
	       :reader evaluator-history-in)
   (history-out :initform (make-array 64 :fill-pointer 0 :adjustable t)
		:reader evaluator-history-out)))

(defvar *evaluator* nil)

(defun take-history-in (hist-ref)
  (let ((history-in (slot-value cl-jupyter::*evaluator* 'cl-jupyter::history-in)))
    (let ((href (if (< hist-ref 0)
                    (+ (+ (length history-in) 1) hist-ref)
                  hist-ref)))
      (if (and (>= href 1)
               (<= href (length history-in)))
          (aref history-in (- href 1))
        nil))))

(defun take-history-out (hist-ref &optional value-ref)
  (let ((history-out (slot-value cl-jupyter::*evaluator* 'cl-jupyter::history-out)))
    (let ((href (if (< hist-ref 0)
                    (+ (+ (length history-out) 1) hist-ref)
		    hist-ref)))
      (when (and (>= href 1)
                 (<= href (length history-out)))
        (let ((out-values  (aref history-out (- href 1))))
          (if value-ref
              (when (and (>= value-ref 1)
                         (<= value-ref (length out-values)))
                (elt out-values (- value-ref 1)))
              (values-list out-values)))))))

(defun make-evaluator (kernel)
  (let ((evaluator (make-instance 'evaluator
				  :kernel kernel)))
    (setf (slot-value kernel 'evaluator) evaluator)
    evaluator))

;;; macro taken from: http://www.cliki.net/REPL
;;; modified to handle warnings correctly
(defmacro handling-errors (&body body)
  `(handler-case
       (handler-bind
           ((simple-warning
              #'(lambda (wrn)
                  (format *error-output* "~&~A: ~%" (class-name (class-of wrn)))
                  (apply (function format) *error-output*
                         (simple-condition-format-control   wrn)
                         (simple-condition-format-arguments wrn))
                  (format *error-output* "~&")
                  (muffle-warning)))
            (warning
              #'(lambda (wrn)
                  (format *error-output* "~&~A: ~%  ~A~%"
                          (class-name (class-of wrn)) wrn)
                  (muffle-warning)))
            (serious-condition
              #'(lambda (err)
                  (logg 2 "~a~%" (with-output-to-string (*standard-output*)
                                   (format t "~a~%" err)))
                  (logg 2 "~a~%" (fredo-utils:backtrace-as-string)))))
         (progn ,@body))
     (simple-condition (err)
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&"))
     (serious-condition (err)
       (format *error-output* "~&An error occurred of type: ~A: ~%  ~S~%"
               (class-name (class-of err)) err))))

(defun evaluate-code (evaluator code)
  ;; (format t "[Evaluator] Code to evaluate: ~W~%" code)
  (logg 2 "[Evaluator] Code to evaluate: ~W  cl-jupyter::*kernel*->~a~%"
                                  code cl-jupyter:*kernel*)
  (let ((execution-count (+ (length (evaluator-history-in evaluator)) 1)))
    (let ((code-to-eval (handler-case
                         (read-from-string (format nil "(progn ~A~%)" code))
                          (END-OF-FILE (err) (list :read-error-eof (format nil "~A" (class-name (class-of err)))))
                          #+clasp(condition (err)
                                   (list :read-error (format nil "~A (condition of type ~A)" err (class-name (class-of err)))))
                         #+sbcl (SB-INT:SIMPLE-READER-ERROR (err)
                                                            (list :read-error (format nil "~A (condition of type ~A)" err (class-name (class-of err))))))))
      ;;(format t "code-to-eval = ~A~%" code-to-eval)
      (cond
       ((and (consp code-to-eval)
             (eq (car code-to-eval) :read-error-eof))
        (values execution-count nil ""
                (format nil "Reader error: incomplete input (condition of type: ~A)~%" (cadr code-to-eval))))
       ((and (consp code-to-eval)
             (eq (car code-to-eval) :read-error))
        (values execution-count nil ""
                (format nil "Reader error: ~A~%" (cadr code-to-eval))))
       ((equal code-to-eval '(progn))
        (values execution-count nil "" (format nil "Warning: no evaluable input~%")))
       ((and (consp code-to-eval)
              (eql (car code-to-eval) 'quicklisp-client:quickload)
              (stringp (cadr code-to-eval)))
         ;; quicklisp hook
         (let ((results (multiple-value-list (ql:quickload (cadr code-to-eval)))))
           (values execution-count results "" "")))
        (t
         ;; else "normal" evaluation
         ;;(format t "[Evaluator] Code to evaluate: ~W~%" code-to-eval)
         (let* ((stdout-str (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
                (stderr-str (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
           (let ((results (with-output-to-string (stdout stdout-str)
                            (with-output-to-string (stderr stderr-str)
                              (let ((*standard-output* stdout)
                                    (*error-output* stderr))
                                (handling-errors
                                        ;(if (and (consp code-to-eval)
                                        ;       (eql (car code-to-eval) 'quicklisp-client:quickload)
                                        ;       (stringp (cadr code-to-eval)))
                                 ;; quicklisp hook
                                        ;  (multiple-value-list (ql:quickload (cadr code-to-eval)))
                                 ;; normal evaluation
                                 (let ((*evaluator* evaluator))
                                   (let ((* (take-history-out -1))
                                         (** (take-history-out -2))
                                         (*** (take-history-out -3)))
                                     ;; put the evaluator in the environment
                                     (logg 2 "In evaluator.lisp:133  cl-jupyter:*kernel* -> ~a~%" cl-jupyter:*kernel*)
                                     (multiple-value-list (eval code-to-eval))))))))))
             ;;(format t "[Evaluator] : results = ~W~%" results)
             (logg 2 "[Evaluator] : results = ~W~%" results)
             (let ((in-code (format nil "~A" code-to-eval)))
               (vector-push-extend (subseq in-code 7 (1- (length in-code)))
                                   (evaluator-history-in evaluator)))
             (vector-push-extend results (evaluator-history-out evaluator))
             (values execution-count results stdout-str stderr-str))))))))
