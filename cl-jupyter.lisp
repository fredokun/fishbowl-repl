;; not yet installed in quicklisp directory
(push (truename (format nil "~Asrc/" (directory-namestring *load-truename*)))
      asdf:*central-registry*)

(let ((cmd-args
       ;; Borrowed from apply-argv, command-line-arguments.  Temporary solution (?)
       ;; This is not PvE's code.
       #+sbcl (cdr sb-ext:*posix-argv*) ; remove the program argument
       #+clozure CCL:*UNPROCESSED-COMMAND-LINE-ARGUMENTS*  ;(ccl::command-line-arguments)
       #+gcl si:*command-args*
       #+(or ecl clasp) (loop for i from 0 below (si:argc) collect (si:argv i))
       #+cmu extensions:*command-line-strings*
       #+allegro (sys:command-line-arguments)
       #+lispworks sys:*line-arguments-list*
       #+clisp ext:*args*
       #-(or sbcl clozure gcl ecl clasp cmu allegro lispworks clisp)
       (error "get-argv not supported for your implementation")))
  (when (= (length cmd-args) 0)
    (progn
      (format t "... initialization mode... please wait...~%")
      (ql:quickload "cl-jupyter")
      (format t "... initialization done...~%")
      #+sbcl (sb-ext:exit :code 0)
      #+(or openmcl mcl) (ccl::quit)
      #-(or sbcl openmcl mcl)
      (error 'not-implemented :proc (list 'quit code)))) 
  (when (not (>= (length cmd-args) 3))
    (error "Wrong number of arguments (given ~A, expecting at least 3)" (length cmd-args)))
  (let ((def-dir (truename (car (last cmd-args 3)))))
    ;;(run-dir (truename (cadr cmd-args))))
    ;; add the source directory to the ASDF registry
    ;; (format t "Definition dir = ~A~%" def-dir)
    (push def-dir asdf:*central-registry*)))


;; for debugging only:
;; (push (truename "./src/") asdf:*central-registry*)

;; activate debugging
(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

;; in production (?)
;;(declaim (optimize (speed 3) (space 0) (debug 0) (safety 2)))

(ql:quickload "cl-jupyter")

(in-package #:cl-jupyter-user)

;;; Look for a COMMON-LISP-USER::CONFIGURE-JUPYTER function to configure the jupyter notebook
(let ((startup-fn (find-symbol "CONFIGURE-JUPYTER" :common-lisp-user)))
  (when startup-fn
    (funcall startup-fn)))

;; start main loop
(cl-jupyter:kernel-start)

