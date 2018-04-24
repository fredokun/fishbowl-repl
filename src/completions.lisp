;;; This file contains code copied (with small changes) from swank.lisp (Public Domain)
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(in-package #:cl-jupyter-swank)

(defconstant keyword-package (find-package :keyword)
  "The KEYWORD package.")

(defun cat (&rest strings)
  "Concatenate all arguments and make the result a string."
  (with-output-to-string (out)
    (dolist (s strings)
      (etypecase s
        (string (write-string s out))
        (character (write-char s out))))))

(defun simple-completions (prefix package)
  "Return a list of completions for the string PREFIX."
  (let ((strings (all-completions prefix package)))
    (list strings (longest-common-prefix strings))))

(defun all-completions (prefix package)
  (multiple-value-bind (name pname intern) (tokenize-symbol prefix)
    (let* ((extern (and pname (not intern)))
	   (pkg (cond ((equal pname "") keyword-package)
                      ((not pname) (guess-buffer-package package))
                      (t (guess-package pname))))
	   (test (lambda (sym) (prefix-match-p name (symbol-name sym))))
	   (syms (and pkg (matching-symbols pkg extern test)))
           (strings (loop for sym in syms
                          for str = (unparse-symbol sym)
                          when (prefix-match-p name str) ; remove |Foo|
                          collect str)))
      (format-completion-set strings intern pname))))

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

(defun unparse-symbol (symbol)
  (let ((*print-case* (case (readtable-case *readtable*) 
                        (:downcase :upcase)
                        (t :downcase))))
    (unparse-name (symbol-name symbol))))

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

(defun format-completion-set (strings internal-p package-name)
  "Format a set of completion strings.
Returns a list of completions with package qualifiers if needed."
  (mapcar (lambda (string) (untokenize-symbol package-name internal-p string))
          (sort strings #'string<)))

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

(defun parse-package (string)
  "Find the package named STRING.
Return the package or nil."
  ;; STRING comes usually from a (in-package STRING) form.
  (ignore-errors
    (find-package (read-from-string string))))

(defun unparse-name (string)
  "Print the name STRING according to the current printer settings."
  ;; this is intended for package or symbol names
  (subseq (prin1-to-string (make-symbol string)) 2))

(defun guess-package (string)
  "Guess which package corresponds to STRING.
Return nil if no package matches."
  (when string
    (or (find-package string)
        (parse-package string)
        (if (find #\! string)           ; for SBCL
            (guess-package (substitute #\- #\! string))))))

;; FIXME: deal with #\| etc.  hard to do portably.
(defun tokenize-symbol (string)
  "STRING is interpreted as the string representation of a symbol
and is tokenized accordingly. The result is returned in three
values: The package identifier part, the actual symbol identifier
part, and a flag if the STRING represents a symbol that is
internal to the package identifier part. (Notice that the flag is
also true with an empty package identifier part, as the STRING is
considered to represent a symbol internal to some current package.)"
  (let ((package (let ((pos (position #\: string)))
                   (if pos (subseq string 0 pos) nil)))
        (symbol (let ((pos (position #\: string :from-end t)))
                  (if pos (subseq string (1+ pos)) string)))
        (internp (not (= (count #\: string) 1))))
    (values symbol package internp)))

(defun untokenize-symbol (package-name internal-p symbol-name)
  "The inverse of TOKENIZE-SYMBOL.

  (untokenize-symbol \"quux\" nil \"foo\") ==> \"quux:foo\"
  (untokenize-symbol \"quux\" t \"foo\")   ==> \"quux::foo\"
  (untokenize-symbol nil nil \"foo\")    ==> \"foo\"
"
  (cond ((not package-name) 	symbol-name)
        (internal-p 		(cat package-name "::" symbol-name))
        (t 			(cat package-name ":" symbol-name))))

(defun guess-buffer-package (string)
  "Return a package for STRING. 
Fall back to the current if no such package exists."
  (or (and string (guess-package string))
      *package*))
