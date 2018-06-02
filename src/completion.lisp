#| completion.lisp

Provide symbol completion for Tab and Shift-Tab in jupyter notebook/lab
- Christian Schafmeister 2018

The symbol completion code was lifted from slime.
|#


(in-package :cl-jupyter)

(defclass completion ()
  ((name :initarg :name :accessor name)
   (metadata :initarg :metadata :accessor metadata)))

(defconstant keyword-package (find-package :keyword)
  "The KEYWORD package.")

(defun simple-completions (prefix package sep-char)
  "Return a list of completions for the string **prefix**.
jupyter lab allows some sort of metadata to be associated with completion
entries - but I don't know how they work and I couldn't find any documentation on it."
  (logg 2 "simple-completions prefix: ~a  package: ~a~%" prefix package)
  (let* ((completions (all-completions prefix package sep-char))
         (strings (mapcar #'name completions))
         (metadata (remove nil (mapcar #'metadata completions))))
    (values (list strings (longest-common-prefix strings)) metadata)))

(defun all-completions (prefix package sep-char)
  (cond
    ((char= sep-char #\")
     (let* ((filename prefix))
       (filename-completions prefix)))
    (t (symbol-completions prefix package))))

(defun filename-completions (prefix)
  (let* ((filename prefix)
         (files (mapcar  (lambda (x)
                           (make-instance 'completion
                                          :name (enough-namestring x)))
                         (append (directory (concatenate 'string filename "*/"))
                                 (directory (concatenate 'string filename "*.*"))
                                 (directory (concatenate 'string filename "*"))))))
    (logg 2 "all-completions  filename: ~a~%" filename)
    files))

(defun symbol-completions (prefix package)
  (logg 2 "symbol-completions for prefix: ~s  package: ~s~%" prefix package)
  (multiple-value-bind (name package-name intern)
      (tokenize-symbol prefix)
    (let* ((extern (and package-name (not intern)))
           (pkg (cond ((equal package-name "") keyword-package)
                      ((not package-name) (guess-buffer-package package))
                      (t (guess-package package-name))))
           (_ (logg 2 "Guessed package: ~a from package-name: ~a~%" pkg package-name))
           (test (lambda (sym) (prefix-match-p name (symbol-name sym))))
           (syms (and pkg (matching-symbols pkg extern test)))
           (completions (loop for sym in syms
                              ;; Once we figure out how the jupyter lab
                              ;; metadata works for completion - we could
                              ;; provide something here
                              for metadata = nil
                              for str = (unparse-symbol sym)
                              when (prefix-match-p name str) ; remove |Foo|
                                collect (make-instance 'completion
                                                       :name str
                                                       :metadata metadata))))
      (format-completion-set completions intern package-name))))

(defun matching-symbols (package external test)
  (let ((test (if external 
                  (lambda (s)
                    (and (symbol-external-p s package) 
                         (funcall test s)))
                  test))
        (result '()))
    (do-symbols (s package)
      (when (funcall test s) 
        (push s result)))
    (remove-duplicates result)))

;; FIXME: this docstring is more confusing than helpful.
(defun symbol-status (symbol &optional (package (symbol-package symbol)))
  "Returns one of 

  :INTERNAL  if the symbol is _present_ in PACKAGE as an _internal_ symbol,

  :EXTERNAL  if the symbol is _present_ in PACKAGE as an _external_ symbol,

  :INHERITED if the symbol is _inherited_ by PACKAGE through USE-PACKAGE,
             but is not _present_ in PACKAGE,

  or NIL     if SYMBOL is not _accessible_ in PACKAGE.


Be aware not to get confused with :INTERNAL and how \"internal
symbols\" are defined in the spec; there is a slight mismatch of
definition with the Spec and what's commonly meant when talking
about internal symbols most times. As the spec says:

  In a package P, a symbol S is
  
     _accessible_  if S is either _present_ in P itself or was
                   inherited from another package Q (which implies
                   that S is _external_ in Q.)
  
        You can check that with: (AND (SYMBOL-STATUS S P) T)
  
  
     _present_     if either P is the /home package/ of S or S has been
                   imported into P or exported from P by IMPORT, or
                   EXPORT respectively.
  
                   Or more simply, if S is not _inherited_.
  
        You can check that with: (LET ((STATUS (SYMBOL-STATUS S P)))
                                   (AND STATUS 
                                        (NOT (EQ STATUS :INHERITED))))
  
  
     _external_    if S is going to be inherited into any package that
                   /uses/ P by means of USE-PACKAGE, MAKE-PACKAGE, or
                   DEFPACKAGE.
  
                   Note that _external_ implies _present_, since to
                   make a symbol _external_, you'd have to use EXPORT
                   which will automatically make the symbol _present_.
  
        You can check that with: (EQ (SYMBOL-STATUS S P) :EXTERNAL)
  
  
     _internal_    if S is _accessible_ but not _external_.

        You can check that with: (LET ((STATUS (SYMBOL-STATUS S P)))
                                   (AND STATUS 
                                        (NOT (EQ STATUS :EXTERNAL))))
  

        Notice that this is *different* to
                                 (EQ (SYMBOL-STATUS S P) :INTERNAL)
        because what the spec considers _internal_ is split up into two
        explicit pieces: :INTERNAL, and :INHERITED; just as, for instance,
        CL:FIND-SYMBOL does. 

        The rationale is that most times when you speak about \"internal\"
        symbols, you're actually not including the symbols inherited 
        from other packages, but only about the symbols directly specific
        to the package in question.
"
  (when package     ; may be NIL when symbol is completely uninterned.
    (check-type symbol symbol) (check-type package package)
    (multiple-value-bind (present-symbol status)
        (find-symbol (symbol-name symbol) package)
      (and (eq symbol present-symbol) status))))


(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "True if SYMBOL is external in PACKAGE.
If PACKAGE is not specified, the home package of SYMBOL is used."
  (eq (symbol-status symbol package) :external))

(defun unparse-symbol (symbol)
  (let ((*print-case* (case (readtable-case *readtable*) 
                        (:downcase :upcase)
                        (t :downcase))))
    (unparse-name (symbol-name symbol))))

(defun unparse-name (string)
  "Print the name STRING according to the current printer settings."
  ;; this is intended for package or symbol names
  (subseq (prin1-to-string (make-symbol string)) 2))


(defun prefix-match-p (prefix string)
  "Return true if PREFIX is a prefix of STRING."
  (not (mismatch prefix string :end2 (min (length string) (length prefix))
                 :test #'char-equal)))

(defun longest-common-prefix (strings)
  "Return the longest string that is a common prefix of STRINGS."
  (if (null strings)
      ""
      (flet ((common-prefix (s1 s2)
               (let ((diff-pos (mismatch s1 s2)))
                 (if diff-pos (subseq s1 0 diff-pos) s1))))
        (reduce #'common-prefix strings))))

(defun format-completion-set (completions internal-p package-name)
  "Format a set of completion strings.
Returns a list of completions with package qualifiers if needed."
  (mapcar (lambda (completion)
            (let ((string (name completion))
                  (metadata (metadata completion)))
              (make-instance 'completion
                             :name (untokenize-symbol package-name internal-p string)
                             :metadata metadata)))
          (sort completions (lambda (x y) (string< (name x) (name y))))))

(defun untokenize-symbol (package-name internal-p symbol-name)
  "The inverse of TOKENIZE-SYMBOL.

  (untokenize-symbol \"quux\" nil \"foo\") ==> \"quux:foo\"
  (untokenize-symbol \"quux\" t \"foo\")   ==> \"quux::foo\"
  (untokenize-symbol nil nil \"foo\")    ==> \"foo\"
"
  (cond ((not package-name)     symbol-name)
        (internal-p             (concatenate 'string package-name "::" symbol-name))
        (t                      (concatenate 'string package-name ":" symbol-name))))

(defun guess-package (string)
  "Guess which package corresponds to STRING.
Return nil if no package matches."
  (when string
    (or (find-package string)
        (parse-package string)
        (if (find #\! string)           ; for SBCL
            (guess-package (substitute #\- #\! string))))))

(defun parse-package (string)
  "Find the package named STRING.
Return the package or nil."
  ;; STRING comes usually from a (in-package STRING) form.
  (ignore-errors
    (find-package (let ((*package* (find-package :keyword)))
                    (read-from-string string)))))

(defun guess-buffer-package (string)
  "Return a package for STRING. 
Fall back to the current if no such package exists."
  (or (and string (guess-package string))
      *package*))

(defun tokenize-symbol (string)
  "STRING is interpreted as the string representation of a symbol
and is tokenized accordingly. The result is returned in three
values: The package identifier part, the actual symbol identifier
part, and a flag if the STRING represents a symbol that is
internal to the package identifier part. (Notice that the flag is
also true with an empty package identifier part, as the STRING is
considered to represent a symbol internal to some current package.)"
  (logg 2 "tokenize-symbol ~a~%" string)
  (let ((package (let ((pos (position #\: string)))
                   (if pos (subseq string 0 pos) nil)))
        (symbol (let ((pos (position #\: string :from-end t)))
                  (if pos (subseq string (1+ pos)) string)))
        (internp (not (= (count #\: string) 1))))
    (logg 2 "tokenize-symbol result -> symbol: ~a  package: ~a  internp: ~a~%" symbol package internp)
    (values symbol package internp)))

(defun fulfil-inspect-request (token)
  (when (and (stringp token) (> (length token) 0))
    (multiple-value-bind (name package-name intern)
        (tokenize-symbol token)
      (let* ((package (if package-name
                          (find-package (let ((*package* (find-package :keyword)))
                                          (read-from-string package-name)))
                          (package-name *package*)))
             (symbol-name (let ((*package* (find-package :keyword)))
                            (string (read-from-string name))))
             (symbol (find-symbol symbol-name package)))
        (cond
          ((fboundp symbol)
           (let* ((func (fdefinition symbol))
                  (lambda-list (fredo-utils:function-lambda-list func))
                  (lambda-list-string (if lambda-list
                                          (string-downcase (format nil "~a" lambda-list))
                                          "()"))
                  (docstring (or (documentation func 'function)
                                 "No documentation available")))
             (list (cons "text/plain" (format nil  "Function       ~a~%~%~a~%" lambda-list-string docstring)))))
          (t nil))))))
