(defpackage #:fredokun-utilities
  (:nicknames #:fredo-utils)
  (:use #:cl)
  (:export #:[]
           #:[]-contains
           #:*example-enabled*
           #:*example-equal-predicate*
           #:example
           #:example-progn
           #:*logg-enabled*
           #:*logg-level*
           #:logg
           #:vbinds
           #:afetch
	   #:while
	   #:read-file-lines
	   #:read-string-file
	   #:read-binary-file
           #:backtrace-as-string
           #:function-lambda-list
	   #:quit))

(defpackage #:myjson
  (:use #:cl #:fredo-utils)
  (:export #:parse-json
	   #:parse-json-from-string
	   #:encode-json
	   #:encode-json-to-string
           #:dumps))

(defpackage #:cl-jupyter
  (:use #:cl #:fredo-utils #:myjson)
  (:export
   #:send-execute-raw-display-object ; cl-jupyter-widgets uses this
   #:display
   #:display-plain render-plain
   #:display-html render-html
   #:display-markdown render-markdown
   #:display-latex render-latex
   #:display-png render-png
   #:display-jpeg render-jpeg
   #:display-svg render-svg
   #:display-json render-json
   #:display-javascript render-javascript
   #:message-header
   #:message-content
   #:message-buffers
   #:message
   #:*shell*
   #:*kernel*
   #:*parent-msg*
   #:*default-special-bindings*
   #:*special-variables*
   #:kernel-start
   #:[]
   #:[]-contains
   #:*kernel-start-hook*
   #:*kernel-shutdown-hook*
   #:*sort-encoded-json*
   #:*handle-comm-open-hook*
   #:*handle-comm-msg-hook*
   #:*handle-comm-close-hook*
   #:*cl-jupyter-widget-display-hook*
   ))

(defpackage #:cl-jupyter-user
  (:use #:cl #:fredo-utils #:cl-jupyter #:common-lisp-user)
  (:export 
   #:display
   #:display-plain render-plain
   #:display-html render-html
   #:display-markdown render-markdown
   #:display-latex render-latex
   #:display-png render-png
   #:display-jpeg render-jpeg
   #:display-svg render-svg
   #:display-json render-json
   #:display-javascript render-javascript
   #:html #:latex #:svg
   #:png-from-file
   #:svg-from-file
   #:quit))

(in-package #:cl-jupyter)
