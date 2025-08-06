(defpackage #:flime/core/text-document
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:range)
  (:import-from #:rope)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:str #:coalton-library/string)
   (#:char #:coalton-library/char)
   (#:file #:coalton-library/file)
   (#:fun #:coalton-library/functions)
   (#:cell #:coalton-library/cell)
   (#:tpl #:coalton-library/tuple)
   (#:indent #:flime/core/sexp/indent))
  (:export #:TextDocument
           #:Version
           #:text-document-path
           #:text-document-version
           #:text-document-language-id
           #:make-text-document
           #:Point
           #:make-point
           #:Range
           #:make-range
           #:range-start
           #:range-end
           #:point-line
           #:point-column
           #:ContentChange
           #:make-incremental-change
           #:make-full-change
           #:apply-content-change
           #:write-text-document
           #:find-symbol-before
           #:find-symbol-on
           #:text-document-package-name
           #:TextEdit
           #:text-edit-range
           #:text-edit-new-text
           #:indent-line-at
           #:format-text-document))
(in-package #:flime/core/text-document)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-type-alias Version UFix)

  (define-struct TextDocument
    (path file:Pathname)
    (language-id (Optional String))
    (version Version)
    (content rope:Rope))

  (declare text-document-path (TextDocument -> file:Pathname))
  (define (text-document-path doc)
    (.path doc))

  (declare text-document-version (TextDocument -> Version))
  (define (text-document-version doc)
    (.version doc))

  (declare text-document-language-id (TextDocument -> (Optional String)))
  (define (text-document-language-id doc)
    (.language-id doc))

  ;; NOTE: This doesn't check if those contents are the same.
  (define-instance (Eq TextDocument)
    (define (== a b)
      (and (== (.path a) (.path b))
           (== (.version a) (.version b)))))

  (define-instance (Into TextDocument rope:Rope)
    (define (into doc)
      (.content doc)))

  (define-instance (Into TextDocument String)
    (define (into doc)
      (as String (.content doc))))

  (declare make-text-document (file:Pathname -> (Optional String) -> Version -> String -> TextDocument))
  (define (make-text-document path language-id version content)
    (TextDocument path
                  language-id
                  version
                  (into content)))

  (define-struct Point
    (line UFix)
    (column UFix))

  (define (make-point line column)
    (Point line column))

  (declare point-line (Point -> UFix))
  (define point-line .line)
  (declare point-column (Point -> UFix))
  (define point-column .column)

  (define-instance (Eq Point)
    (define (== a b)
      (and (== (.line a) (.line b))
           (== (.column a) (.line b)))))

  (define-instance (Ord Point)
    (define (<=> a b)
      (match (<=> (.line a) (.line b))
        ((EQ)
         (<=> (.column a) (.column b)))
        (x x))))

  (define-type-alias Position UFix)

  (define-struct Range
    (start Point)
    (end Point))

  (declare make-range (Point -> Point -> Range))
  (define (make-range start end)
    (Range start end))

  (declare range-start (Range -> Point))
  (define range-start .start)
  (declare range-end (Range -> Point))
  (define range-end .end)

  (define-type ContentChange
    (FullContentChange String)
    (IncrementalContentChange String Range))

  (define-type ParseError
    (EndOfFile Unit)
    (UnbalancedClosingParen UFix))

  (declare %skip-n-lines! (iter:Iterator Char -> UFix -> UFix -> (Result ParseError UFix)))
  (define (%skip-n-lines! iter n pos)
    (if (== n 0)
        (Ok pos)
        (match (iter:index-of! (== #\Newline) iter)
          ((Some index)
           (%skip-n-lines! iter (1- n) (1+ (+ pos index))))
          ((None)
           (Err (EndOfFile))))))

  (define (skip-n-lines! iter n)
    (%skip-n-lines! iter n 0))

  (define (skip-n-columns! iter n)
    (for pos in (iter:up-to n)
      (match (iter:next! iter)
        ((Some #\Newline)
         (return pos))
        ((Some _) (continue))
        ((None)
         (return (1- pos)))))
    n)

  (define (map-while!* f iter)
    (let done = (cell:new False))
    (iter:map-while! (fn (elem)
                       (let ((res (f elem)))
                         (if (none? res)
                             (progn
                               (cell:write! done True)
                               (Some elem))
                             res)))
                     (iter:new (fn ()
                                 (if (cell:read done)
                                     None
                                     (iter:next! iter))))))

  (declare read-next-line ((Iterator Char) -> (Result ParseError (Iterator Char))))
  (define (read-next-line iter)
    (match (iter:next! iter)
      ((Some ch)
       (Ok
        (map-while!* (fn (ch)
                       (if (== #\Newline ch)
                           None
                           (Some ch)))
                     (iter:chain! (iter:once ch) iter))))
      ((None) (Err (EndOfFile)))))

  (define (lines iter)
    (match (read-next-line iter)
      ((Ok line)
       (iter:chain!
        (iter:once (the String (iter:collect! line)))
        (lines iter)))
      ((Err _) iter:empty)))

  (declare get-position-at-point (TextDocument -> Point -> (Result ParseError Position)))
  (define (get-position-at-point doc point)
    (let ((iter (iter:into-iter (.content doc))))
      (match (skip-n-lines! iter (.line point))
        ((Ok pos)
         (Ok (+ pos (skip-n-columns! iter (.column point)))))
        ((Err e) (Err e)))))


  (declare get-point-at-position (TextDocument -> UFix -> Point))
  (define (get-point-at-position doc pos)
    (let iter = (iter:into-iter (.content doc)))
    (let line = (the (Cell UFix) (cell:new 0)))
    (let col = (the (Cell UFix) (cell:new 0)))
    (with-default (Point 0 0)
      (iter:last!
       (iter:map-while! (fn (x)
                          (match x
                            ((Tuple i ch)
                             (cond
                               ((< pos i)
                                None)
                               (True
                                (let ((res (Some (Point (cell:read line)
                                                        (cell:read col)))))
                                  (match ch
                                    (#\Newline
                                     (cell:increment! line)
                                     (cell:write! col 0)
                                     Unit)
                                    (_
                                     (cell:increment! col)
                                     Unit))
                                  res))))))
                        (iter:enumerate! iter)))))

  (declare apply-content-change (TextDocument -> ContentChange -> Version -> TextDocument))
  (define (apply-content-change doc change new-version)
    (match change
      ((IncrementalContentChange text rang)
       (if (< new-version (.version doc))
           doc
           (match (Tuple (get-position-at-point doc (.start rang))
                         (get-position-at-point doc (.end rang)))
             ((Tuple (Ok start-pos)
                     (Ok end-pos))
              (let (Tuple before rope2) = (rope:cut (.content doc) start-pos))
              (let (Tuple _ after) = (rope:cut rope2 (- end-pos start-pos)))
              (TextDocument
               (.path doc)
               (.language-id doc)
               new-version
               (fold rope:append
                     before
                     (make-list
                      (as rope:Rope text)
                      after))))
             (_ doc))))
      ((FullContentChange text)
       (TextDocument (.path doc) (.language-id doc) new-version (into text)))))

  (define (make-incremental-change text start end)
    (IncrementalContentChange text (Range start end)))

  (define (make-full-change text)
    (FullContentChange text))

  (declare write-rope-to-stream ((file:FileStream Char) -> rope:Rope -> (Result file:FileError Unit)))
  (define (write-rope-to-stream stream rope)
    (match rope
      ((rope::Leaf _ str)
       (file:write-string stream str))
      ((rope::Branch _ l r)
       (write-rope-to-stream stream l)
       (write-rope-to-stream stream r))))

  (declare write-text-document ((file:FileStream Char) -> TextDocument -> (Result file:FileError Unit)))
  (define (write-text-document stream doc)
    (write-rope-to-stream stream (into doc)))

  (declare newline? (Char -> Boolean))
  (define (newline? char)
    (== char #\Newline))

  (declare whitespace? (Char -> Boolean))
  (define (whitespace? char)
    (list:member char (make-list #\Space #\Tab #\Page #\Return)))

  (declare special-char? (Char -> Boolean))
  (define (special-char? char)
    (list:member char (make-list #\( #\) #\" #\' #\` #\, #\. #\; #\\ #\#)))

  (declare separator? (Char -> Boolean))
  (define separator?
    (fun:reduce fun:disjoin newline? (make-list whitespace? special-char?)))

  (define (starts-with prefix iter)
    (== (iter:count!
         (iter:map-while! (fn ((Tuple a b))
                            (if (== a b)
                                (Some (Tuple a b))
                                None))
                          (iter:zip! (str:chars prefix) iter)))
        (str:length prefix)))

  (declare read-until-without-escape (Char -> (Iterator (Tuple UFix Char)) -> UFix -> (Tuple String (Tuple UFix UFIx))))
  (define (read-until-without-escape char iter last-newline-pos)
    (let ((done (cell:new False))
          (escaped (cell:new False))
          (line (cell:new 0))
          (newline-pos (cell:new last-newline-pos)))
      (Tuple
       (the String
            (iter:collect!
             (map tpl:snd
                  (map-while!* (fn (x)
                                 (match x
                                   ((Tuple pos c)
                                    (cond
                                      ((cell:read escaped)
                                       (cell:write! escaped False)
                                       (Some x))
                                      ((== c char)
                                       (cell:write! done True)
                                       None)
                                      ((== c #\\)
                                       (if (cell:read escaped)
                                           (cell:write! escaped False)
                                           (cell:write! escaped True))
                                       (Some x))
                                      ((== c #\Newline)
                                       (cell:increment! line)
                                       (cell:write! newline-pos (1+ pos))
                                       (Some x))
                                      (True (Some x))))))
                               iter))))
       (Tuple (cell:read line)
              (cell:read newline-pos)))))

  (define (read-symbol iter)
    (let ((last (cell:new iter:empty))
          (inside-bar (cell:new False))
          (symb (iter:map-while! (fn (x)
                                   (match x
                                     ((Tuple _ ch)
                                      (cond
                                        ((and (not (cell:read inside-bar))
                                              (separator? ch))
                                         (cell:write! last (iter:once x))
                                         None)
                                        ((== #\| ch)
                                         (cell:update-swap! not inside-bar)
                                         (Some ch))
                                        (True
                                         (Some ch))))))
                                 iter)))
      (Tuple
       (the String (iter:collect! symb))
       (iter:chain!
        (cell:read last)
        iter))))

  (define (read-digits iter)
    (let ((last (cell:new iter:empty))
          (digits (iter:map-while! (fn (x)
                                     (match x
                                       ((Tuple _ ch)
                                        (if (char:ascii-digit? ch)
                                            (Some ch)
                                            (progn
                                              (cell:write! last (iter:once x))
                                              None)))))
                                   iter)))
      (Tuple
       (the String (iter:collect! digits))
       (iter:chain!
        (cell:read last)
        iter))))

  (define-struct Token
    (string String)
    (position UFix)
    (line UFix)
    (column UFix))

  (declare unenumerated ((Optional (Tuple UFix Char)) -> (Optional Char)))
  (define unenumerated (unwrap-or-else (fun:compose Some tpl:snd) (fn () None)))

  (define-struct ReadTokenProgress
    (iter (Iterator (Tuple UFix Char)))
    (start-line UFix)
    (end-line UFix)
    (last-newline-pos UFix))

  (declare read-next-token ((Iterator (Tuple UFix Char)) -> UFix -> UFix -> (Optional (Tuple Token ReadTokenProgress))))
  (define (read-next-token iter line last-newline-pos)
    (let ((calc-col (fn (pos)
                      (if (== pos 0)
                          0
                          (- pos last-newline-pos)))))
      (match (iter:find! (fun:compose (fun:complement whitespace?) tpl:snd) iter)
        ((Some (Tuple pos #\#))
         (let column = (calc-col pos))
         (match (unenumerated (iter:next! iter))
           ;; Block comment
           ((Some #\|)
            (let line-number = (cell:new line))
            (let newline-pos = (cell:new last-newline-pos))
            (while-let (Some (Tuple pos ch)) = (iter:next! iter)
              (cond
                ((== #\Newline ch)
                 (cell:increment! line-number)
                 (cell:write! newline-pos (1+ pos))
                 Unit)
                ((== #\| ch)
                 (match (iter:next! iter)
                   ((Some (Tuple _ #\#))
                    (break))
                   ((Some (Tuple pos #\Newline))
                    (cell:increment! line-number)
                    (cell:write! newline-pos (1+ pos))
                    (continue))
                   (_ (continue))))
                (True (continue))))
            (read-next-token iter (cell:read line-number) (cell:read newline-pos)))
           ;; Character literal
           ((Some #\\)
            (match (unenumerated (iter:next! iter))
              ((Some ch)
               (Some (Tuple (Token (as String (make-list #\# #\\ ch)) pos line column)
                            (ReadTokenProgress iter line line last-newline-pos))))
              ((None)
               (Some (Tuple (Token "#\\" pos line column)
                            (ReadTokenProgress iter line line last-newline-pos))))))
           ;; Uninterned symbol
           ((Some #\:)
            (match (read-symbol iter)
              ((Tuple symb next-iter)
               (Some (Tuple (Token (str:concat "#:" symb) pos line column)
                            (ReadTokenProgress next-iter line line last-newline-pos))))))
           ;; TODO: Array literal
           ;; Other #-prefixed dispatch macro
           ((Some ch)
            (Some (Tuple (Token (as String (make-list #\# ch)) pos line column)
                         (ReadTokenProgress iter line line last-newline-pos))))
           ((None)
            (Some (Tuple (Token "#" pos line column)
                         (ReadTokenProgress iter line line last-newline-pos))))))
        ;; Inline comment
        ((Some (Tuple _ #\;))
         (match (iter:find! (fun:compose newline? tpl:snd) iter)
           ((Some (Tuple pos #\Newline))
            (read-next-token iter (1+ line) (1+ pos)))
           ((Some _) None)
           ((None) None)))
        ;; Vertical bar (|) symbol
        ((Some (Tuple pos #\|))
         (let column = (calc-col pos))
         (match (read-until-without-escape #\| iter last-newline-pos)
           ((Tuple str (Tuple newlines newline-pos))
            (Some (Tuple (Token str pos line column)
                         (ReadTokenProgress iter line (+ line newlines) newline-pos))))))
        ;; String literal
        ((Some (Tuple pos #\"))
         (let column = (calc-col pos))
         (match (read-until-without-escape #\" iter last-newline-pos)
           ((Tuple str (Tuple newlines newline-pos))
            (Some (Tuple (Token str pos line column)
                         (ReadTokenProgress iter line (+ line newlines) newline-pos))))))
        ;; Comma
        ((Some (Tuple pos #\,))
         (let column = (calc-col pos))
         (match (iter:next! iter)
           ((Some (Tuple _ #\@))
            (Some (Tuple (Token ",@" pos line column)
                         (ReadTokenProgress iter line line last-newline-pos))))
           ((Some x)
            (Some (Tuple (Token "," pos line column)
                         (ReadTokenProgress (iter:chain! (iter:once x) iter) line line last-newline-pos))))
           ((None)
            (Some (Tuple (Token "," pos line column)
                         (ReadTokenProgress iter line line last-newline-pos))))))
        ;; Dot (Dotted-pair or a floating point)
        ((Some (Tuple pos #\.))
         (let column = (calc-col pos))
         ;; Parse as a float if the next character is an integer.
         (match (iter:next! iter)
           ((Some (Tuple i ch))
            (if (char:ascii-digit? ch)
                ;; TODO: Allow postfix like `d0` for double-float.
                (match (read-digits iter)
                  ((Tuple digits next-iter)
                   (Some (Tuple (Token (str:concat (as String ch) digits) pos line column)
                                (ReadTokenProgress next-iter line line last-newline-pos)))))
                (Some (Tuple (Token "." pos line column)
                             (ReadTokenProgress (iter:chain! (iter:once (Tuple i ch)) iter)
                                                line line last-newline-pos)))))
           ((None) (Some (Tuple (Token "." pos line column)
                                (ReadTokenProgress iter line line last-newline-pos))))))
        ;; Newline
        ((Some (Tuple pos #\Newline))
         (read-next-token iter (1+ line) (1+ pos)))
        ;; Other characters (maybe symbol)
        ((Some (Tuple pos ch))
         (let column = (calc-col pos))
         (if (special-char? ch)
             (Some (Tuple (Token (as String ch) pos line column)
                          (ReadTokenProgress iter line line last-newline-pos)))
             (match (read-symbol iter)
               ((Tuple symb next-iter)
                (Some (Tuple (Token (str:concat (as String ch) symb) pos line column)
                             (ReadTokenProgress next-iter line line last-newline-pos)))))))
        ((None) None))))

  (define (tokens iter)
    (let ((main (fn (iter line last-newline-pos)
                  (match (read-next-token iter line last-newline-pos)
                    ((Some (Tuple tok (ReadTokenProgress next-iter _start-line end-line newline-pos)))
                     (iter:chain!
                      (iter:once tok)
                      (main next-iter end-line newline-pos)))
                    ((None) iter:empty)))))
      (main iter 0 0)))

  (declare read-next-form ((Iterator Token) -> (Result ParseError (List Token))))
  (define (read-next-form tokens-iter)
    (match (iter:next! tokens-iter)
      ((Some next-token)
       (cond
         ((list:member (.string next-token)
                       (make-list "'" "`" "," ",@"))
          (Ok
           (cons next-token (with-default Nil (read-next-form tokens-iter)))))
         ((== "(" (.string next-token))
          (let ((level (the (Cell UFix) (cell:new 0)))
                (last (cell:new iter:empty))
                (form
                  (the (List Token)
                       (iter:collect!
                        (iter:map-while! (fn (token)
                                           (match (.string token)
                                             ("("
                                              (cell:increment! level)
                                              (Some token))
                                             (")"
                                              (cond
                                                ((== 0 (cell:read level))
                                                 (cell:write! last (iter:once token))
                                                 None)
                                                (True
                                                 (cell:decrement! level)
                                                 (Some token))))
                                             (_ (Some token))))
                                         tokens-iter))))
                (last-elems (the (List Token) (iter:collect! (cell:read last)))))
            (if (and (list:null? form)
                     (list:null? last-elems))
                (Ok (singleton next-token))
                (Ok (cons next-token (list:append form last-elems))))))
         ((== ")" (.string next-token))
          (Err (UnbalancedClosingParen (.position next-token))))
         (True (Ok (singleton next-token)))))
      ((None) (Err (EndOfFile)))))

  (define (forms tokens-iter)
    (match (read-next-form tokens-iter)
      ((Ok form)
       (iter:chain!
        (iter:once form)
        (forms tokens-iter)))
      ((Err _) iter:empty)))

  (declare find-text-document-package-name (TextDocument -> (Optional String)))
  (define (find-text-document-package-name doc)
    (match (.language-id doc)
      ((Some lang)
       (match lang
         ("coalton"
          (let ((content-tokens
                  (the (List String)
                       (iter:collect!
                        (iter:take! 3
                                    (map
                                     .string
                                     (tokens
                                      (iter:enumerate! (iter:into-iter (.content doc))))))))))
            (match content-tokens
              ((Cons "(" (Cons "package" (Cons package-name _)))
               (Some package-name))
              (_ None))))
         ("lisp"
          (let ((content-tokens (tokens (iter:enumerate! (iter:into-iter (.content doc))))))
            (match
                (iter:find! (fn (form)
                              (and (== "(" (.string (list:car form)))
                                   (list:member (.string (list:nth 1 form))
                                                (make-list "in-package"
                                                           "cl:in-package"))))
                            (forms content-tokens))
              ((Some in-package-form)
               (if (list:null? (list:nth-cdr 2 in-package-form))
                   None
                   (let ((package-name-token (list:nth 3 in-package-form))
                         (package-name (.string package-name-token))
                         (iter (str:chars package-name)))
                     (match (iter:next! iter)
                       ((Some #\#)
                        (match (iter:next! iter)
                          ((Some #\:)
                           (Some (the String (iter:collect! iter))))
                          (_
                           (Some package-name))))
                       ((Some #\:)
                        (Some (the String (iter:collect! iter))))
                       (_
                        (Some package-name))))))
              ((None) None))))
         (_ None)))
      ((None) None)))

  (define (text-document-package-name doc)
    (match (find-text-document-package-name doc)
      ((Some package-name) package-name)
      ((None)
       (match (.language-id doc)
         ((Some "common-lisp") "COMMON-LISP-USER")
         ((Some "coalton") "COALTON-USER")
         (_ "COALTON-USER")))))

  ;; FIXME: This doesn't care if the point is in a string or a comment.
  ;;   To fix it, the comprehensive parser is required to parse from the toplevel forms.
  (declare find-symbol-before (TextDocument -> Point -> (Result ParseError String)))
  (define (find-symbol-before doc point)
    (let ((iter (iter:into-iter (.content doc))))
      (match (skip-n-lines! iter (.line point))
        ((Ok _pos)
         (let ((text-before-point (the String (iter:collect! (iter:take! (.column point) iter)))))
           (Ok
            (match
                (iter:index-of! separator?
                                (str:chars (str:reverse text-before-point)))
              ((Some index)
               (let ((len (str:length text-before-point)))
                 (str:substring text-before-point
                                (- len index)
                                len)))
              ((None) text-before-point)))))
        ((Err e) (Err e)))))

  (declare find-symbol-on (TextDocument -> Point -> (Result ParseError (Optional String))))
  (define (find-symbol-on doc point)
    (let ((iter (iter:into-iter (.content doc))))
      (match (get-position-at-point doc point)
        ((Ok pos)
         (match (iter:find! (fn (token)
                              (and (<= (.position token) pos)
                                   (<= pos (+ (str:length (.string token)) (.position token)))))
                            (tokens (iter:enumerate! iter)))
           ((Some tok)
            (let ((first-char (str:ref-unchecked (.string tok) 0)))
              (if (or (separator? first-char)
                      (char:ascii-digit? first-char))
                  (Ok None)
                  (Ok (Some (.string tok))))))
           ((None) (Err (EndOfFile)))))
        ((Err e) (Err e)))))

  (declare find-toplevel-form-at ((Iterator (List Token)) -> UFix -> (List Token)))
  (define (find-toplevel-form-at forms pos)
    (let ((last-form (the (Cell (List Token)) (cell:new Nil))))
      (while-let (Some form) = (iter:next! forms)
        (when (< pos (.position (list:car form)))
          (return (cell:read last-form)))
        (match (list:last form)
          ((Some last-token)
           (when (<= pos (.position last-token))
             (return form)))
          ((None) Unit))
        (cell:write! last-form form))
      (return (cell:read last-form))))

  (declare text-document-toplevel-form-at (TextDocument -> Point -> (Result ParseError (List Token))))
  (define (text-document-toplevel-form-at doc point)
    (match (get-position-at-point doc point)
      ((Ok pos)
       (Ok (find-toplevel-form-at
            (forms (tokens (iter:enumerate! (iter:into-iter (.content doc)))))
            pos)))
      ((Err e) (Err e))))

  (define (split-by f l)
    (match l
      ((Cons x xs)
       (if (f x)
           (match (split-by f xs)
             ((Tuple before after)
              (Tuple (cons x before) after)))
           (Tuple Nil l)))
      (_ (Tuple Nil Nil))))

  (declare find-parent-form ((List Token) -> UFix -> (Optional (List Token))))
  (define (find-parent-form tokens pos)
    (match (split-by (fn (token)
                       (< (.position token) pos))
                     tokens)
      ((Tuple before-tokens after-tokens)
       (let level = (cell:new 0))
       (match
           (read-next-form
            (iter:chain!
             (iter:into-iter
              (reverse
               (the (List Token)
                    (iter:collect!
                     (map-while!* (fn (token)
                                    (match (.string (the Token token))
                                      ("(" (if (== (cell:read level) 0)
                                               None
                                               (progn
                                                 (cell:decrement! level)
                                                 (Some token))))
                                      (")"
                                       (cell:increment! level)
                                       (Some token))
                                      (_ (Some token))))
                                  (iter:into-iter (reverse before-tokens)))))))
             (iter:into-iter after-tokens)))
         ((Ok parent-form)
          (Some parent-form))
         ((Err _) None)))))

  (declare find-effective-form-at ((List Token) -> UFix -> (Optional (List Token))))
  (define (find-effective-form-at tokens pos)
    (match (find-parent-form tokens pos)
      ((Some parent-form)
       (match (find-parent-form tokens (.position (list:car parent-form)))
         ((Some (Cons x (Cons y _ys)))
          (if (and (== (.string x) "(")
                   (== (.string y) "("))
              (match (find-parent-form tokens (.position x))
                ((Some ancestor-form)
                 (Some ancestor-form))
                ((None)
                 (Some parent-form)))
              (Some parent-form)))
         (_ (Some parent-form))))
      ;; Toplevel
      ((None) None)))

  (define (text-document-effective-form-at doc point)
    (match (get-position-at-point doc point)
      ((Ok pos)
       (let ((toplevel-form
               (find-toplevel-form-at
                (forms (tokens (iter:enumerate! (iter:into-iter (.content doc)))))
                pos)))
         (Ok (find-effective-form-at toplevel-form pos))))
      ((Err e) (Err e))))

  (define (%form-elements tokens-iter)
    (match (read-next-form tokens-iter)
      ((Ok next-form)
       (iter:chain!
        (iter:once next-form)
        (%form-elements tokens-iter)))
      (_ iter:empty)))

  (declare group-form-elements ((List Token) -> (Iterator (List Token))))
  (define (group-form-elements form)
    (when (or (list:null? form)
              (not (== "(" (.string (list:car form)))))
      (return (iter:once form)))
    (%form-elements (iter:into-iter (list:cdr form))))

  (declare beginning-of-line (TextDocument -> Point -> (Tuple UFix Point)))
  (define (beginning-of-line doc p)
    (let ((padding (the (Cell UFix) (cell:new 0))))
      (Tuple
       (iter:sum!
        (iter:map-while! (fn (item)
                           (match item
                             ((Tuple i line)
                              (cond
                                ((< (.line p) i) None)
                                ((== (.line p) i)
                                 (cell:write! padding
                                              (iter:count!
                                               (iter:map-while! (fn (x)
                                                                  (if (whitespace? x)
                                                                      (Some x)
                                                                      None))
                                                                (str:chars line))))
                                 (Some (cell:read padding)))
                                (True
                                 (Some (str:length line)))))))
                         (iter:enumerate! (lines (iter:into-iter (.content doc))))))
       (Point (.line p) (cell:read padding)))))

  (declare get-form-path-at ((List Token) -> UFix -> (List UFix)))
  (define (get-form-path-at form pos)
    (match (iter:last!
            (iter:map-while! (fn (item)
                               (match item
                                 ((Tuple _i elems)
                                  (if (<= pos (.position (list:car elems)))
                                      None
                                      (Some item)))))
                             (iter:enumerate! (group-form-elements form))))
      ((Some (Tuple i subform))
       (cond
         ((list:null? subform)
          Nil)
         ((== "(" (.string (list:car subform)))
          (match (list:last subform)
            ((Some token-right-before)
             (if (< (.position token-right-before) pos)
                 (singleton (1+ i))
                 (cons i (get-form-path-at subform pos))))
            ((None)
             (singleton (1+ i)))))
         (True
          (singleton (1+ i)))))
      ((None) Nil)))

  (declare calc-indent-level (String -> (List UFix) -> (Optional UFix)))
  (define (calc-indent-level fun-name path)
    ;; Don't count the first function symbol.
    (let path = (cons (1- (list:car path)) (list:cdr path)))
    (lisp (Optional UFix) (fun-name path)
      (cl:let ((level (indent:calc-indent-level fun-name path)))
        (cl:if level
               (Some level)
               None))))

  (define-struct TextEdit
    (range Range)
    (new-text String))

  (declare text-edit-range (TextEdit -> Range))
  (define text-edit-range .range)
  (declare text-edit-new-text (TextEdit -> String))
  (define text-edit-new-text .new-text)

  (declare group-tokens-by-line ((Iterator Token) -> (Iterator (List Token))))
  (define (group-tokens-by-line tokens)
    (let ((take-line (fn (tokens line-num)
                       (let next = (cell:new None))
                       (Tuple
                        (the (List Token)
                             (iter:collect!
                              (iter:map-while! (fn (token)
                                                 (if (== (.line (the Token token)) line-num)
                                                     (Some token)
                                                     (progn
                                                       (cell:write! next (Some token))
                                                       None)))
                                               tokens)))
                        (cell:read next)))))
      (match (iter:next! tokens)
        ((Some first-token)
         (match (take-line tokens (.line first-token))
           ((Tuple same-line-tokens next-token)
            (iter:chain!
             (iter:once
              (cons first-token
                    same-line-tokens))
             (group-tokens-by-line
              (iter:chain!
               (unwrap-or-else iter:once
                               (fn () iter:empty)
                               next-token)
               tokens))))))
        ((None) iter:empty))))

  (declare calc-base-col ((List Token) -> UFix -> UFix))
  (define (calc-base-col form pos)
    (match (find-parent-form form pos)
      ((Some parent-form)
       (.column (list:car parent-form)))
      ((None)
       (.column (list:car form)))))

  (declare format-line-in-form ((List Token) -> (List TextEdit)))
  (define (format-line-in-form form)
    (let edits = (the (Cell (List TextEdit)) (cell:new Nil)))
    (iter:fold! (fn (acc tokens)
                  (if (list:null? acc)
                      tokens
                      (let ((pos (.position (the Token (list:car tokens))))
                            (indent-level
                              (match (find-effective-form-at acc pos)
                                ((Some (Cons x xs))
                                 (let ((base-col (calc-base-col (cons x xs) pos)))
                                   (cond
                                     ((== "(" (.string x))
                                      (match xs
                                        ((Cons fun-token ys)
                                         (if (== "(" (.string fun-token))
                                             (.column fun-token)
                                             (let ((path (get-form-path-at (cons x xs) pos)))
                                               (match (calc-indent-level (.string fun-token) path)
                                                 ((Some level)
                                                  (+ base-col level))
                                                 ((None)
                                                  (if (<= 2 (list:car path))
                                                      ;; Same level as the first argument
                                                      (.column (list:car ys))
                                                      (+ base-col 1)))))))
                                        ((Nil) (+ base-col 1))))
                                     (True base-col))))
                                (_ 0))))
                        (list:append
                         acc
                         (if (== indent-level (.column (list:car tokens)))
                             tokens
                             (progn
                               (cell:push! edits
                                           (TextEdit (make-range (Point (.line (list:car tokens)) 0)
                                                                 (Point (.line (list:car tokens))
                                                                        (.column (list:car tokens))))
                                                     (lisp String (indent-level)
                                                       (cl:make-string indent-level :initial-element #\Space))))
                               (map (fn (tok)
                                      (Token (.string tok)
                                             (+ (- (.position tok)
                                                   (.column (list:car tokens)))
                                                indent-level)
                                             (.line tok)
                                             (+ (- (.column tok)
                                                   (.column (list:car tokens)))
                                                indent-level)))
                                    tokens)))))))
                Nil
                (group-tokens-by-line (iter:into-iter form)))
    (cell:read edits))

  (declare indent-line-at (TextDocument -> Point -> (List TextEdit)))
  (define (indent-line-at doc p)
    (let (Tuple pos beginning-point) = (beginning-of-line doc p))
    (let ((toplevel-form
            (find-toplevel-form-at
             (forms (tokens (iter:enumerate! (iter:into-iter (.content doc)))))
             pos))
          (indent-level
            (match (find-effective-form-at toplevel-form pos)
              ((Some (Cons x xs))
               (let ((base-col (calc-base-col (cons x xs) pos))
                     (indent-level
                       (cond
                         ((== "(" (.string x))
                          (match xs
                            ((Cons fun-token ys)
                             (if (== "(" (.string fun-token))
                                 (.column fun-token)
                                 (let ((path (get-form-path-at (cons x xs) pos)))
                                   (match (calc-indent-level (.string fun-token) path)
                                     ((Some level)
                                      (+ base-col level))
                                     ((None)
                                      (if (<= 2 (list:car path))
                                          ;; Same level as the first argument
                                          (.column (list:car ys))
                                          (+ base-col 1)))))))
                            ((Nil) (+ base-col 1))))
                         (True base-col))))
                 indent-level))
              ;; Assuming it's at toplevel.
              (_ 0))))
      (make-list (TextEdit (make-range (Point (.line p) 0)
                                       beginning-point)
                           (lisp String (indent-level)
                             (cl:make-string indent-level :initial-element #\Space))))))

  (declare format-text-document (TextDocument -> (List TextEdit)))
  (define (format-text-document doc)
    (let ((content-tokens (tokens (iter:enumerate! (iter:into-iter (.content doc))))))
      (iter:collect!
       (iter:flatten!
        (map (fn (form)
               (iter:into-iter (format-line-in-form form)))
             (forms content-tokens)))))))
