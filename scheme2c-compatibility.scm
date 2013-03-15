(module scheme2c-compatibility * 
(import chicken scheme srfi-1 foreign posix lolevel extras traversal data-structures)
(use posix lolevel foreigners xlib ports files srfi-13 srfi-14)

;; http://paste.call-cc.org/paste?id=16706e0fe1ae3eecb85762a87ebbff295c8a7632#a2

(define (unsigned-list->unsigneda l)
 (let ((buffer (allocate (* (length l) c-sizeof-long))))
  (for-each-indexed
   (lambda (v i) (c-longunsigned-set! buffer (* i c-sizeof-long) v))
   l)
  buffer))

;; Really qobischeme
(define (eleventh x) (caddr (cddddr (cddddr x))))
(define (twelfth x) (cadddr (cddddr (cddddr x))))

(define (xor a b) (if a (not b) b))

(define (usleep microseconds) ((foreign-lambda void "usleep" integer) microseconds))

(define (fuck-up) (error "fuck-up"))

;;; Temporary files

(define *tmp* "/tmp")

(define (tmp pathname) (string-append *tmp* "/" pathname))

(define (with-temporary-file _ f)
 (let* ((name (create-temporary-file))
        (result (f name)))
  (delete-file* name)
  result))

;;; Strings

(define (lines string) (string-split string "\n" #t))
(define (unlines l) (string-join l "\n"))
(define (words string) (string-split string " " #t))
(define (unwords l) (string-join l " "))
(define (number-of-fields string) (length (fields string)))
(define (field-ref string n) (list-ref (fields string) n))
(define (fields string)
 (string-tokenize string (char-set-complement char-set:whitespace)))

;;; Pathnames

;;; needs work: missing notions: ., .., foo~, foo.~n~, .foo, #foo#, /foo/, and
;;;             foo/

(define *panic?* #t)

(define *program* #f)

(define (panic format-string . &rest)
 (cond (*panic?*
	(format (current-error-port) "~a: ~a~%"
		*program* (apply format #f format-string &rest))
	(exit -1))
       (else (apply error 'panic format-string &rest))))

(define (has-directory? pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (let loop ((l (reverse (string->list pathname))))
  (and (not (null? l)) (or (char=? (first l) #\/) (loop (rest l))))))

(define (get-directory pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (unless (has-directory? pathname) (panic "No directory"))
 (let ((l (string->list pathname)))
  (substring pathname 0 (- (length l) (positionv #\/ (reverse l)) 1))))

(define (strip-directory pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (if (has-directory? pathname)
     (let ((l (string->list pathname)))
      (substring pathname
		 (- (length l) (positionv #\/ (reverse l)))
		 (length l)))
     pathname))

(define (directory-list d) (directory d))

(define (has-extension? pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (let loop ((l (reverse (string->list pathname))))
  (and (not (null? l))
       (not (char=? (first l) #\/))
       (or (char=? (first l) #\.) (loop (rest l))))))

(define (extension pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (unless (has-extension? pathname) (panic "No extension"))
 (substring pathname
	    (- (string-length pathname)
	       (positionv #\. (reverse (string->list pathname))))
	    (string-length pathname)))

(define (strip-extension pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (let loop ((l (reverse (string->list pathname))))
  (cond ((or (null? l) (char=? (first l) #\/)) pathname)
	((char=? (first l) #\.) (list->string (reverse (rest l))))
	(else (loop (rest l))))))

(define (default-extension pathname extension)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (if (has-extension? pathname)
     pathname
     (string-append pathname "." extension)))

(define (replace-extension pathname extension)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (string-append (strip-extension pathname) "." extension))

(define (read-object-from-file pathname)
 (if (string=? pathname "-") (read) (call-with-input-file pathname read)))

(define (write-object-to-file object pathname)
 (cond ((string=? pathname "-") (pp object) (newline))
       (else (call-with-output-file pathname
	      (lambda (port) (pp object port) (newline port))))))

(define (read-text-file pathname)
 (if (string=? pathname "-")
     (let loop ((lines '()) (line (read-line)))
      (if (eof-object? line)
	  (reverse lines)
	  (loop (cons line lines) (read-line))))
     (call-with-input-file pathname read-lines)))

(define (write-text-file lines pathname)
 (if (string=? pathname "-")
     (for-each (lambda (line) (write-line line)) lines)
     (call-with-output-file pathname
      (lambda (port)
       (for-each (lambda (line) (write-line line port)) lines)))))

(define (read-from-string string) (with-input-from-string string read))

;;; End from QobiScheme

;; Almost compatible with idealib-c-macros.sch:
;; s/unsigned/unsigned int/
;; s/string/c-string/
;; s/pointer/c-pointer/
;; Query replace regexp (default c-value \(.*?\) \(.*?\)) -> c-value \1 "\2")):
;;
;; Note that + and = do not work on pointers, use pointer+ and pointer=?

(define getenv get-environment-variable)

(define-syntax c-value
 (syntax-rules ()
  ((_ type name)
   (foreign-value name type))))
(define-syntax c-sizeof
 (syntax-rules ()
  ((_ type)
   (foreign-type-size type))))
(define-syntax c-function
 (syntax-rules ()
  ((_ return-type (name arguments ...))
   (foreign-lambda return-type name arguments ...))))

(define bit-not bitwise-not)
(define bit-and bitwise-and)
(define bit-or bitwise-ior)
(define (bit-lsh n o) (arithmetic-shift n o))
(define (bit-rsh n o) (arithmetic-shift n (- o)))

(define flush-buffer flush-output)

(define c-sizeof-short (foreign-type-size "short"))
(define c-sizeof-int (foreign-type-size "int"))
(define c-sizeof-long (foreign-type-size "long"))
(define c-sizeof-float (foreign-type-size "float"))
(define c-sizeof-double (foreign-type-size "double"))
;; This is intentionally not defined because any code that uses it
;; will eventually fail in any system but Scheme->C 
;; (define c-sizeof-tscp (foreign-type-size "void*"))
(define c-sizeof-s2cuint (foreign-type-size "void*"))
(define c-sizeof-pointer (foreign-type-size "void*"))

(define (pointer-pointer-ref ptr-to-ptr)
 ((foreign-lambda* c-pointer ((c-pointer ptr)) "C_return(*(void**)ptr);") ptr-to-ptr))

(define (pointer-pointer-set! ptr-to-ptr val)
 ((foreign-lambda* 
   void
   ((c-pointer ptr) (c-pointer value))
   "*(void**)ptr = value;"
   "C_return(0);")
  ptr-to-ptr val))

(define (c-string->string ptr)
 ((foreign-lambda* c-string ((c-pointer string)) "C_return(string);") ptr))

;; Scheme->C SRFI-4 compatibility
(define (c-byte-ref ptr off) (pointer-u8-ref (pointer+ ptr off)))
(define (c-shortint-ref ptr off) (pointer-s16-ref (pointer+ ptr off)))
(define (c-shortunsigned-ref ptr off) (pointer-u16-ref (pointer+ ptr off)))
(define (c-int-ref ptr off) (pointer-s32-ref (pointer+ ptr off)))
(define (c-unsigned-ref ptr off) (pointer-u32-ref (pointer+ ptr off)))
(define (c-longint-ref ptr off) 
 ((foreign-lambda* long ((c-pointer ptr)) "C_return(*(long*)ptr);")
  (pointer+ ptr off)))
(define (c-longunsigned-ref ptr off) 
 ((foreign-lambda* unsigned-long ((c-pointer ptr)) "C_return(*(long*)ptr);")
  (pointer+ ptr off)))
(define (c-s2cuint-ref ptr off) (pointer-pointer-ref (pointer+ ptr off)))
;; This is intentionally not defined because any code that uses it
;; will eventually fail in any system but Scheme->C 
;; c-tscp-ref
(define (c-float-ref ptr off) (pointer-f32-ref (pointer+ ptr off)))
(define (c-double-ref ptr off) (pointer-f64-ref (pointer+ ptr off)))
(define (c-byte-set! ptr off val) (pointer-u8-set! (pointer+ ptr off) val))
(define (c-shortint-set! ptr off val) (pointer-s16-set! (pointer+ ptr off) val))
(define (c-shortunsigned-set! ptr off val) (pointer-u16-set! (pointer+ ptr off) val))
(define (c-int-set! ptr off val) (pointer-s32-set! (pointer+ ptr off) val))
(define (c-unsigned-set! ptr off val) (pointer-u32-set! (pointer+ ptr off) val))
(define (c-longint-set! ptr off val) 
 ((foreign-lambda* void ((c-pointer ptr) (long val))
                   "*(long*)ptr = val;"
                   "C_return(0);")
  (pointer+ ptr off) val))
(define (c-longunsigned-set! ptr off val) 
 ((foreign-lambda* void ((c-pointer ptr) (unsigned-long val))
                   "*(long*)ptr = val;"
                   "C_return(0);")
  (pointer+ ptr off) val))
(define (c-s2cuint-set! ptr off val) (pointer-pointer-set! (pointer+ ptr off) val))
;; This is intentionally not defined because any code that uses it
;; will eventually fail in any system but Scheme->C 
;; c-tscp-set!
;; TODO Why does this setfault without exact->inexact when passing in an exact?
(define (c-float-set! ptr off val) (pointer-f32-set! (pointer+ ptr off) (exact->inexact val)))
(define (c-double-set! ptr off val) (pointer-f64-set! (pointer+ ptr off) (exact->inexact val)))

;; toollib-c-bindings compatibility 
(define malloc allocate)
(define memcpy move-memory!)
(define bzero (c-function void ("bzero" c-pointer int)))
(define (with-alloc x f)
 (let* ((data (malloc x))
        (r (begin (bzero data x) (f data))))
  (free data)
  r))

(define (with-file-stream f filename mode)
 (let* ((file (fopen filename mode))
	(result (f file)))
  (fclose file)
  result))

(define (with-buffer-stream f buffer size mode)
 (let* ((file (fmemopen buffer size mode))
	(result (f file)))
  (fclose file)
  result))

(define fclose (c-function void ("fclose" c-pointer)))
(define fopen (c-function c-pointer ("fopen" c-string c-string)))
(define fmemopen (c-function c-pointer ("fmemopen" c-pointer unsigned-int c-string)))

(define (with-c-string str f)
 (with-alloc (+ (string-length str) 1)
	     (lambda (buf)
	      (for-each-indexed
	       (lambda (c i) (c-byte-set! buf i (char->integer c)))
	       (string->list str))
              (c-byte-set! buf (string-length str) 0)
	      (f buf (+ (string-length str) 1)))))

(define (c-null-separated-strings->strings c-strings)
 (let loop ((c-strings c-strings) (strings '()) (i 0))
  (if (pointer=? (address->pointer 0) (c-s2cuint-ref c-strings 0))
      (reverse strings)
      (loop (pointer+ c-strings (c-sizeof "void*"))
	    (cons (c-string->string (c-s2cuint-ref c-strings 0))
		  strings)
	    (+ i 1)))))

(define (with-vector->c-array f set-element element-size v)
 (with-array (vector-length v) element-size
	     (lambda (array)
	      (f (vector->c-array array v set-element element-size)))))

(define (with-c-pointers f v)
 (with-vector->c-array f c-s2cuint-set! c-sizeof-s2cuint v))

(define (list->c-array array l set-element element-size)
 (define (for-each-indexed f l)
  (let loop ((i 0) (l l))
   (unless (null? l) (f (first l) i) (loop (+ i 1) (cdr l)))))
 (for-each-indexed (lambda (x i) (set-element array (* i element-size) x)) l)
 array)

(define (list->c-inexact-array array l element-size signed?)
  (list->c-array
   array
   l
   (c-sized-inexact-ptr-set! element-size signed?)
   element-size))

(define (list->c-exact-array array l element-size signed?)
  (list->c-array
   array
   l
   (c-sized-int-ptr-set! element-size signed?)
   element-size))

(define (vector->c-array array v set-element element-size)
 (for-each-vector
  (lambda (x i) (set-element array (* i element-size) x))
  v
  (enumerate-vector (vector-length v)))
 array)

(define (vector->c-inexact-array array v element-size signed?)
 (vector->c-array
  array
  v
  (c-sized-inexact-ptr-set! element-size signed?)
  element-size))

(define (vector->c-exact-array array v element-size signed?)
 (vector->c-array
  array
  v
  (c-sized-int-ptr-set! element-size signed?)
  element-size))

(define (with-array elements element-size f)
  (with-alloc (* elements element-size) f))

(define (c-array->list array get-element element-size nr-elements)
  (vector->list (c-array->vector array get-element element-size nr-elements)))

(define (c-array->vector array get-element element-size nr-elements)
 (map-n-vector
   (lambda (x) (get-element array (* x element-size))) nr-elements))

(define (c-exact-array->list array element-size nr-elements signed?)
  (vector->list
   (c-exact-array->vector array element-size nr-elements signed?)))

(define (c-exact-array->vector array element-size nr-elements signed?)
  (c-array->vector
   array
   (c-sized-int-ptr-ref element-size signed?) element-size nr-elements))

(define (c-inexact-array->list array element-size nr-elements signed?)
  (vector->list
   (c-inexact-array->vector array element-size nr-elements signed?)))

(define (c-inexact-array->vector array element-size nr-elements signed?)
  (c-array->vector
   array
   (c-sized-inexact-ptr-ref element-size signed?) element-size nr-elements))

(define (c-sized-int-ptr-ref size signed?)
  (cond
   ((= size 1) c-byte-ref)
   ((= size c-sizeof-short) (if signed? c-shortint-ref c-shortunsigned-ref))
   ((= size c-sizeof-int)   (if signed? c-int-ref c-unsigned-ref))
   ((= size c-sizeof-long)  (if signed? c-longint-ref c-longunsigned-ref))
   (else (error "unknown size" size))))

(define (c-sized-int-ptr-set! size signed?)
  (cond
   ((= size 1) c-byte-set!)
   ((= size c-sizeof-short) (if signed? c-shortint-set! c-shortunsigned-set!))
   ((= size c-sizeof-int)   (if signed? c-int-set! c-unsigned-set!))
   ((= size c-sizeof-long)  (if signed? c-longint-set! c-longunsigned-set!))
   (else (error "unknown size" size))))

(define (c-sized-inexact-ptr-ref size signed?)
  (cond
   ((= size c-sizeof-float) c-float-ref)
   ((= size c-sizeof-double) c-double-ref)
   (else (error "unknown size" size))))

(define (c-sized-inexact-ptr-set! size signed?)
  (cond
   ((= size c-sizeof-float) c-float-set!)
   ((= size c-sizeof-double) c-double-set!)
   (else (error "unknown size" size))))

(define popen open-input-pipe)
(define pclose close-input-pipe)

;;; X11

(foreign-declare "#include \"dither.c\"")

(define (c-docolordither pic24 w h rdisp gdisp bdisp idisp maplen)
 ((foreign-lambda c-pointer
                  "DoColorDither"
                  c-pointer int int c-pointer c-pointer c-pointer c-pointer int)
  pic24 w h rdisp gdisp bdisp idisp maplen))

(foreign-declare "#include <X11/Xlib.h>")

(define (ylookupstring event . opt)
 (let* ((buffer-size 100)
        (buffer (make-blob buffer-size))
        (status (if (= (length opt) 2) 
                    (cadr opt)
                    #f)))
  (let-location
   ((keysym long))
   (let ((size
          ((foreign-lambda int "XLookupString" c-pointer blob int c-pointer c-pointer)
           event buffer buffer-size (location keysym) status)))
    (if (null? opt)
        (substring (blob->string buffer) 0 size)
        (list (substring (blob->string buffer) 0 size)
              (if (car opt) keysym #f)))))))

(define (xallocnamedcolor3 dpy cmap colorname)
 ;; TODO Change this to a let-location
 (let* ((color1 (allocate (foreign-type-size "XColor")))
        (color2 (allocate (foreign-type-size "XColor")))
        (return-value (xallocnamedcolor dpy cmap colorname
                                        color1 color2)))
  (list return-value color1 color2)))

(define (xquerypointer2 dpy w)
 ;; TODO Change this to a let-location
 (let* ((root (allocate (foreign-type-size "Window")))
        (child (allocate (foreign-type-size "Window"))))
  (let-location
   ((root c-pointer)
    (child c-pointer)
    (root-x int)
    (root-y int)
    (win-x int)
    (win-y int)
    (mask unsigned-int))
   (let ((r (xquerypointer dpy w 
                           (location root) 
                           (location child)
                           (location root-x)
                           (location root-y)
                           (location win-x)
                           (location win-y)
                           (location mask))))
    (list r root child root-x root-y win-x win-y mask)))))

)
