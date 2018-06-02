(in-package #:cl-jupyter)

#|

# The shell router socket #

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These variables are always bound when a cell is evaluated
;;; They define a dynamic environment within which SEND operates
(defvar *parent-msg* nil)
(defvar *shell* nil)
(defvar *kernel* nil)
(defvar *default-special-bindings*)
(defvar *special-variables* '(*parent-msg* *shell* *kernel*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These variables are to allow cl-jupyter-widgets to extend
;;; the cl-jupyter message handler.

(defparameter *kernel-start-hook* nil)
(defparameter *kernel-shutdown-hook* nil)
(defparameter *sort-encoded-json* nil)
(defparameter *handle-comm-open-hook* nil)
(defparameter *handle-comm-msg-hook* nil)
(defparameter *handle-comm-close-hook* nil)
(defparameter *cl-jupyter-widget-display-hook* nil
  "A function with the form (display-hook result-widget iopub msg execution-count key).
If the display hook function recognizes result-widget as a widget it displays it and
returns T. If it doesn't recognize it as a widget then simply return NIL and shell will
display the result.")

(defclass shell-channel ()
  ((kernel :initarg :kernel :reader shell-kernel)
   (socket :initarg :socket :initform nil :accessor shell-socket)))


(defun make-shell-channel (kernel)
  (let ((socket (pzmq:socket (kernel-ctx kernel) :router)))
    (let ((shell (make-instance 'shell-channel
                                :kernel kernel
                                :socket socket)))
      (let ((config (slot-value kernel 'config)))
        (let ((endpoint (format nil "~A://~A:~A"
                                  (config-transport config)
                                  (config-ip config)
                                  (config-shell-port config))))
          ;; (format t "shell endpoint is: ~A~%" endpoint)
          (pzmq:bind socket endpoint)
          shell)))))

(defparameter *session-receive-lock* (bordeaux-threads:make-lock "session-receive-lock"))

(defun shell-loop (shell)
  (unwind-protect
       (progn
         (bordeaux-threads:acquire-lock *session-receive-lock*)
         (let ((active t))
           (format t "[Shell] loop started~%")
           (send-status-starting (kernel-iopub (shell-kernel shell)) (kernel-session (shell-kernel shell)) :key (kernel-key shell))
           (format t "[Shell] entering main loop~%")
           (while active
                  (logg 1 "[Shell] top of main loop~%")
                  (vbinds (identities sig msg)  (message-recv (shell-socket shell))
                          (logg 1 "[Shell] Received message ~s~%" msg)
                          (progn
                            (logg 2 "Shell Received message:~%")
                            (logg 2 "  | identities: ~A~%" identities)
                            (logg 2 "  | signature: ~W~%" sig)
                            (logg 2 "  | message: ~A~%" (encode-json-to-string (message-header msg)))
                            (logg 2 "  | number of buffers: ~W~%" (length (message-buffers msg))))
                          ;; TODO: check the signature (after that, sig can be forgotten)
                          (let* ((msg-type (header-msg-type (message-header msg))))
                            (let ((*parent-msg* msg)
                                  (*shell* shell)
                                  (*kernel* (shell-kernel shell)))
                              (let ((*default-special-bindings* (list (cons '*parent-msg* *parent-msg*)
                                                                      (cons '*shell* *shell*)
                                                                      (cons '*kernel* *kernel*))))
                                (logg 2 "  |  *parent-msg* -> ~s~%" *parent-msg*)
                                (cond ((equal msg-type "kernel_info_request")
                                       (handle-kernel-info-request shell identities msg))
                                      ((equal msg-type "execute_request")
                                       (setf active (handle-execute-request shell identities msg)))
                                      ((equal msg-type "comm_open")
                                       (when cl-jupyter:*handle-comm-open-hook*
                                         (funcall cl-jupyter:*handle-comm-open-hook* shell identities msg)))
                                      ((equal msg-type "comm_msg")
                                       (when cl-jupyter:*handle-comm-msg-hook*
                                         (funcall cl-jupyter:*handle-comm-msg-hook* shell identities msg)))
                                      ((equal msg-type "comm_close")
                                       (when cl-jupyter:*handle-comm-close-hook*
                                         (funcall cl-jupyter:*handle-comm-close-hook* shell identities msg)))
                                      ((equal msg-type "complete_request")
                                       (complete-request shell identities msg))
                                      ((equal msg-type "inspect_request")
                                       (inspect-request shell identities msg))
                                      (t (warn "[Shell] message type '~A' not (yet ?) supported, skipping... msg: ~s" msg-type msg))))))))))
    (bordeaux-threads:release-lock *session-receive-lock*)))


#|

### Message type: kernel_info_reply ###

|#


(defun complete-request (shell identities msg)
  "Handle complete_request.  This provides tab completion to cl-jupyter."
  (let* ((json (parse-json-from-string (message-content msg)))
         (text ([] json "code"))
         (cursor-end ([] json "cursor_pos"))
         (sep-pos (position-if (lambda (c) (or (char<= c #\space)
                                               (char= c #\()
                                               (char= c #\))
                                               (char= c #\")
                                               (char= c #\#)
                                               (char= c #\')
                                               (char= c #\`)))
                               text
                               :start 0
                               :end cursor-end
                               :from-end t))
         (cursor-start (if sep-pos (1+ sep-pos) 0))
         (partial-token (subseq text cursor-start cursor-end)))
    (handler-case
        (handler-bind
            ((serious-condition
               #'(lambda (err)
                   (logg 2 "~a~%" (with-output-to-string (*standard-output*)
                                    (format t "~a~%" err)))
                   (logg 2 "~a~%" (fredo-utils:backtrace-as-string)))))
          (multiple-value-bind (completions metadata)
              (simple-completions partial-token *package* (char text sep-pos))        
            (logg 2 "complete-request partial-token: ~a~%" partial-token)
            (logg 2 "   --> completions: ~s~%" completions)
            (logg 2 "   --> metadata: ~s~%" metadata)
            (let* ((data `(("status" . "ok")
                           ("metadata" . ,metadata)
                           ("cursor_start" . ,cursor-start)
                           ("cursor_end" . ,cursor-end)
                           ("matches" . ,(make-array (length (car completions)) :initial-contents (car completions)))))
                   (reply (make-message msg "complete_reply" nil data)))
              (message-send (shell-socket shell) reply :identities identities :key (kernel-key shell)))))
      (simple-condition (err)
        (format *error-output* "~&~A: ~%" (class-name (class-of err)))
        (apply (function format) *error-output*
               (simple-condition-format-control   err)
               (simple-condition-format-arguments err))
        (format *error-output* "~&"))
      (serious-condition (err)
        (format *error-output* "~&An error occurred of type: ~A: ~%  ~S~%"
                (class-name (class-of err)) err)))))


(defun inspect-request (shell identities msg)
  "Handle inspect_request.  This provides Shift-Tab completion to cl-jupyter.
This should be improved so that if the cursor is inside of a form it returns information
on the function of the form.  Currently it provides information on the function associated
with the symbol to the left of the cursor."
  (let* ((json (parse-json-from-string (message-content msg)))
         (text ([] json "code"))
         (cursor-end ([] json "cursor_pos"))
         (sep-pos (position-if (lambda (c) (or (char<= c #\space)
                                               (char= c #\()
                                               (char= c #\))
                                               (char= c #\")
                                               (char= c #\#)
                                               (char= c #\')
                                               (char= c #\`)))
                               text
                               :start 0
                               :end cursor-end
                               :from-end t))
         (cursor-start (if sep-pos (1+ sep-pos) 0))
         (partial-token (subseq text cursor-start cursor-end)))
    (logg 2 "inspect_request partial-token: ~s~%" partial-token)
    (let ((metadata nil)
          (data (fulfil-inspect-request partial-token)))
      (let* ((data `(("status" . "ok")
                     ("found" . ,(if data :true :false))
                     ("metadata" . ,metadata)
                     ("data" . ,data)))
             (reply (make-message msg "inspect_reply" nil data)))
        (message-send (shell-socket shell) reply :identities identities :key (kernel-key shell))))))

;; for protocol version 5  
(defclass content-kernel-info-reply (message-content)
  ((protocol-version :initarg :protocol-version :type string)
   (implementation :initarg :implementation :type string)
   (implementation-version :initarg :implementation-version :type string)
   (language-info-name :initarg :language-info-name :type string)
   (language-info-version :initarg :language-info-version :type string)
   (language-info-mimetype :initarg :language-info-mimetype :type string)
   (language-info-pygments-lexer :initarg :language-info-pygments-lexer :type string)
   (language-info-codemirror-mode :initarg :language-info-codemirror-mode :type string)
   (language-info-nbconvert-exporter :initarg :language-info-nbconvert-exporter :type string)
   (banner :initarg :banner :type string)
   ;; help links: (text . url) a-list
   (help-links :initarg :help-links)))

;; for protocol version 4.1
;;(defclass content-kernel-info-reply (message-content)
;; ((protocol-version :initarg :protocol-version)
;;   (language-version :initarg :language-version)
;;   (language :initarg :language :type string)))

(defun help-links-to-json (help-links)
  (concatenate 'string "["
	       (concat-all 'string ""
			   (join "," (mapcar (lambda (link) 
					       (format nil "{ \"text\": ~W, \"url\": ~W }" (car link) (cdr link))) 
					     help-links)))
	       "]"))

;; for protocol version 5
(defmethod encode-json (stream (object content-kernel-info-reply) &key (indent nil) (first-line nil))
  (with-slots (protocol-version
               implementation implementation-version
               language-info-name language-info-version
               language-info-mimetype language-info-pygments-lexer language-info-codemirror-mode
               language-info-nbconvert-exporter
               banner help-links) object
    (encode-json stream `(("protocol_version" . ,protocol-version)
                          ("implementation" . ,implementation)
                          ("implementation_version" . ,implementation-version)
                          ("language_info" . (("name" . ,language-info-name)
                                              ("version" . ,language-info-version)
                                              ("mimetype" . ,language-info-mimetype)
                                              ("pygments_lexer" . ,language-info-pygments-lexer)
                                              ("codemirror_mode" . ,language-info-codemirror-mode)))
                                              ;("nbconvert_exporter" . ,language-info-nbconvert-exporter)))
                          ("banner" . "cl-jupyter")) ; ,banner)
                          ;("help_links" . ,help-links))
                 :indent indent :first-line first-line)))

(defun kernel-key (shell)
  (kernel-config-key (kernel-config (shell-kernel shell))))

(defun handle-kernel-info-request (shell identities msg)
  ;;(format t "[Shell] handling 'kernel-info-request'~%")
  ;; status to busy
  ;;(send-status-update (kernel-iopub (shell-kernel shell)) msg "busy" :key (kernel-key shell))
  ;; for protocol version 5
  (let ((reply (make-message
                msg "kernel_info_reply" nil
                (make-instance
                 'content-kernel-info-reply
                 :protocol-version (header-version (message-header msg))
                 :implementation +KERNEL-IMPLEMENTATION-NAME+
                 :implementation-version +KERNEL-IMPLEMENTATION-VERSION+
                 :language-info-name "common-lisp"
                 :language-info-version "X3J13"
                 :language-info-mimetype "text/x-common-lisp"
                 :language-info-pygments-lexer "common-lisp"
                 :language-info-codemirror-mode "text/x-common-lisp"
                 :language-info-nbconvert-exporter ""
                 :banner (banner)
                 :help-links (vector)))))
		 ;;'(("Common Lisp Hyperspec" . "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm"))))))
  ;; for protocol version 4.1
  ;; (let ((reply (make-message-from-parent msg "kernel_info_reply" nil 
  ;;   				 (make-instance
  ;;   				  'content-kernel-info-reply
  ;;   				  :protocol-version #(4 1)
  ;;   				  :language-version #(1 2 7)  ;; XXX: impl. dependent but really cares ?
   ;;   				  :language "common-lisp"))))
    (message-send (shell-socket shell) reply :identities identities :key (kernel-key shell))
    ;; status back to idle
    ;;(send-status-update (kernel-iopub (shell-kernel shell)) msg "idle" :key (kernel-key shell))
    ))

#|

### Message type: execute_request ###

|#

(defun handle-execute-request (shell identities msg)
  (logg 2 "[Shell] handling 'execute_request'~%")
  (send-status-update (kernel-iopub (shell-kernel shell)) msg "busy" :key (kernel-key shell))
  (let ((content (parse-json-from-string (message-content msg))))
    (logg 2 "  ==> Message content = ~W~%" content)
    (let ((code (afetch "code" content :test #'equal)))
      (logg 2 "  ===>    Code to execute = ~W~%" code)
      (vbinds (execution-count results stdout stderr)
	      (progn
		(logg 2 "Set cl-jupyter:*kernel* -> ~a ~%" cl-jupyter:*kernel*)
		(evaluate-code (kernel-evaluator (shell-kernel shell)) code))
	      (logg 2 "==> Execution count = ~A~%" execution-count)
	      (logg 2 "==> results = ~S~%" results)
	      (logg 2 "==> STDOUT = ~S~%" stdout)
	      (logg 2 "==> STDERR = ~S~%" stderr)
	      ;; broadcast the code to connected frontends
	      (send-execute-code (kernel-iopub (shell-kernel shell)) msg execution-count code :key (kernel-key shell))
	      (when (and (consp results) (typep (car results) 'cl-jupyter-user::cl-jupyter-quit-obj))
		;; ----- ** request for shutdown ** -----
		(let ((reply (make-message msg "execute_reply" nil
					   `(("status" . "abort")
					     ("execution_count" . ,execution-count)))))
		  (message-send (shell-socket shell) reply :identities identities :key (kernel-key shell)))
		(return-from handle-execute-request nil))
	      ;; ----- ** normal request ** -----
	      ;; send the stdout
	      (when (and stdout (> (length stdout) 0))
		(send-stream (kernel-iopub (shell-kernel shell)) msg "stdout" stdout :key (kernel-key shell)))
	      ;; send the stderr
	      (when (and stderr (> (length stderr) 0))
		(send-stream (kernel-iopub (shell-kernel shell)) msg "stderr" stderr :key (kernel-key shell)))
	      ;; send the first result
              (logg 2 "==> About to display results -> ~s~%" results)
              (cond
                ((and *cl-jupyter-widget-display-hook*
                      (funcall *cl-jupyter-widget-display-hook*
                               (car results)
                               (kernel-iopub (shell-kernel shell))
                               msg execution-count (kernel-key shell))))
                (t (send-execute-result (kernel-iopub (shell-kernel shell))
				       msg execution-count (car results) :key (kernel-key shell))))
	      ;; status back to idle
	      (send-status-update (kernel-iopub (shell-kernel shell)) msg "idle" :key (kernel-key shell))
	      ;; send reply (control)
	      (let ((reply (make-message msg "execute_reply" nil
					 `(("status" . "ok")
					   ("execution_count" . ,execution-count)
					   ("payload" . ,(vector))))))
		(message-send (shell-socket shell) reply :identities identities :key (kernel-key shell))
		t)))))

#|
     
## Message content ##

|#

(defclass message-content ()
  ()
  (:documentation "The base class of message contents."))
