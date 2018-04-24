(in-package #:cl-jupyter)

#|

# The shell router socket #

|#

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

(defun shell-loop (shell)
  "Main kernel loop, which waits for messages from the notebook"
  (let ((active t))
    (format t "[Shell] loop started~%")
    (send-status-starting (kernel-iopub (shell-kernel shell)) (kernel-session (shell-kernel shell)) :key (kernel-key shell))
    (while active
      (vbinds (identities sig msg buffers)  (message-recv (shell-socket shell))
	      ;;(format t "Shell Received:~%")
	      ;;(format t "  | identities: ~A~%" identities)
	      ;;(format t "  | signature: ~W~%" sig)
	      ;;(format t "  | message: ~A~%" (encode-json-to-string (message-header msg)))
	      ;;(format t "  | buffers: ~W~%" buffers)

	      ;; TODO: check the signature (after that, sig can be forgotten)
	      (let ((msg-type (header-msg-type (message-header msg))))
		(cond ((equal msg-type "kernel_info_request")
		       (handle-kernel-info-request shell identities msg buffers))
		      ((equal msg-type "execute_request")
		       (setf active (handle-execute-request shell identities msg buffers)))
                      ((equal msg-type "inspect_request")
                       (handle-inspect-request shell identities msg buffers))
                      ((equal msg-type "complete_request")
                       (handle-complete-request shell identities msg buffers))
		      (t (warn "[Shell] message type '~A' not (yet ?) supported, skipping..." msg-type))))))))


#|

### Message type: kernel_info_reply ###

|#

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

(defun handle-kernel-info-request (shell identities msg buffers)
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


(defun handle-execute-request (shell identities msg buffers)
  ;;(format t "[Shell] handling 'execute_request'~%")
  (send-status-update (kernel-iopub (shell-kernel shell)) msg "busy" :key (kernel-key shell))
  (let ((content (parse-json-from-string (message-content msg))))
    ;;(format t "  ==> Message content = ~W~%" content)
    (let ((code (afetch "code" content :test #'equal)))
      ;;(format t "  ===> Code to execute = ~W~%" code)
      (vbinds (execution-count results stdout stderr)
          (evaluate-code (kernel-evaluator (shell-kernel shell)) code)
        ;(format t "Execution count = ~A~%" execution-count)
        ;(format t "results = ~A~%" results)
        ;(format t "STDOUT = ~A~%" stdout)
        ;(format t "STDERR = ~A~%" stderr)
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
	(send-execute-result (kernel-iopub (shell-kernel shell)) 
			     msg execution-count (car results) :key (kernel-key shell))
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

### Message type: inspect_request ###

|#

(defun get-token (code pos &optional (levels 1))
  "Find a token to look up, based on code string and position.
  
  Look backwards to find the opening brace, then read the first token.
  For a function/macro expression this should be a symbol.

  LEVELS contains the number of opening parentheses to stop at.
  If this is 1 then the innermost form is searched, 2 finds the parent form etc.

  Returns nil as first value if nothing found or an error occurred
  "
  (when (zerop (length code))
    (return-from get-token (values nil 0)))
  
  (let ((start-paren (let ((start-pos (if (char= (char code pos) #\)) ; Ignore ')' at point
                                          (1- pos)
                                          pos))
                           (paren-count levels)) ; Keep track of the number of parentheses
                       ;; Note: This will get confused by literal parentheses in the source code
                       (loop for i from start-pos downto 0 do ; Search backwards through the string
                            (case (char code i)
                              (#\( (when (zerop (decf paren-count)) ; Decrement parenthesis count
                                     (return i)))  ; Found beginning of this form, so return index
                              (#\) (incf paren-count))
                              (otherwise nil))
                          finally (return-from get-token (values nil 0))))))  ; Not found
    
    ;; Read from the string, starting at (start-paren + 1)
    ;; Don't error, just return nil
    (ignore-errors (read-from-string code nil nil :start (1+ start-paren)))))

(example (get-token "" 0) => (values nil 0))
(example (get-token "(" 0) => (values nil 1))
(example (get-token "(test" 0) => (values 'test 5))
(example (get-token "(  test" 5) => (values 'test 7))
; Ignore nested forms before position
(example (get-token "( *test-sym (answer 42) here" 25) => '*test-sym)
; This next example triggers a read error condition
(example (get-token "(let ()" 6) => nil)

(defun get-token-search (code pos &optional (levels 2))
  "Searches for a token using get-token, starting in the innermost form 
   and going outwards up to LEVELS"
  (loop for level from 1 to levels do
       (let ((token (get-token code pos level)))
         (if token (return-from get-token-search token))))
  nil) ; Not found

(example (get-token-search "(  test" 5) => 'test)
(example (get-token-search "(let ()" 6) => 'let)

(defun handle-inspect-request (shell identities msg buffers)
  "Inspection request. Processes a inspect_request message in MSG,
   calling message-send with an inspect_reply type message."
  (format t "[Shell] handling 'inspect_request'~%")
  (let ((content (parse-json-from-string (message-content msg))))
    (format t "  ==> Message content = ~W~%" content)
    (let ((code (afetch "code" content :test #'equal))
          (cursor-pos (afetch "cursor_pos" content :test #'equal))
          (detail-level (afetch "detail_level" content :test #'equal)))

      ;;(format t "  ===> Code to inspect = ~W~%" code)
        
      (let ((reply (let ((text (let ((token (get-token-search code cursor-pos)))
                                 (if (and token (symbolp token))
                                     ;; Get output of describe into a string
                                     (let ((str (make-string-output-stream)))
                                       (describe token str)
                                       (get-output-stream-string str))))))
                     
                     ;; Here text is either nil or a description
                     ;;(format t "  ===> Description = ~W~%" text)
                     
                     (if text
                         (make-message msg "inspect_reply" nil
                                       `(("status" . "ok")
                                         ("found" . :true)
                                         ("data" . (("text/plain" . ,text)
                                                    ("dummy" . "none"))))) ; Note: needed for some reason. JSON map encoding?
                         ;; No text
                         (make-message msg "inspect_reply" nil
                                       `(("status" . "ok")
                                         ("found" . :false)))))))
        
        (message-send (shell-socket shell) reply :identities identities :key (kernel-key shell))
        t))))

#|

### Message type: complete_request ###

|#

(defun symbol-start-before-cursor (code pos)
  "Return the starting position of the symbol which ends
   in the string CODE at position POS"
  (loop for i from (1- pos) downto 0 do
       (let ((ch (char code i)))
         (if (or (char= ch #\Space)
                 (char= ch #\Newline)
                 (char= ch #\Tab)
                 (char= ch #\()
                 (char= ch #\)))
             ;; Start of the symbol. Symbol goes from character i+1 to pos (non-inclusive)
             (return-from symbol-start-before-cursor (1+ i)))))
  0)

(example (symbol-start-before-cursor "test" 3) => 0)
(example (symbol-start-before-cursor "(another thing" 8) => 1)
(example (symbol-start-before-cursor "(another thing" 14) => 9)

(defun handle-complete-request (shell identities msg buffers)
  "Processes a complete_request message in MSG,
   calling message-send with a complete_reply type message."
  (format t "[Shell] handling 'complete_request'~%")
  (let ((content (parse-json-from-string (message-content msg))))

    (format t "  ==> Message content = ~W~%" content)
    
    (let* ((code (afetch "code" content :test #'equal))
           (cursor-pos (afetch "cursor_pos" content :test #'equal))
           (sym-start (symbol-start-before-cursor code cursor-pos))
           (prefix (subseq code sym-start cursor-pos)))
      
      (when (string= "" prefix)
        (return-from handle-complete-request nil)) ; No prefix to match
      
      ;; Find all completions, then sort by length with the shortest first
      (let* ((matches (sort (cl-jupyter-swank:all-completions prefix *package*)
                            #'< :key #'length))
             
             (reply (make-message msg "complete_reply" nil
                                  `(("status" . "ok")
                                    ("matches" . ,(apply #'vector matches)) ; Convert to vector so converted to JSON array
                                    ("cursor_start" . ,sym-start)
                                    ("cursor_end" . ,cursor-pos)))))
        
        (message-send (shell-socket shell) reply :identities identities :key (kernel-key shell))))))

#|
     
## Message content ##

|#

(defclass message-content ()
  ()
  (:documentation "The base class of message contents."))

