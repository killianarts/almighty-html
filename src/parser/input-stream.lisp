;;;; input-stream.lisp — Preprocessed input stream per WHATWG §13.2.3
;;;; Handles preprocessing: CR/LF normalization.

(defpackage #:almighty-html/parser/input-stream
  (:use #:cl)
  (:export
   #:html-input-stream
   #:make-html-input-stream
   #:stream-next-char
   #:stream-peek-char
   #:stream-reconsume
   #:stream-eof-p
   #:stream-match-insensitive
   #:stream-consume-chars
   #:html-input-stream-data
   #:html-input-stream-pos
   #:html-input-stream-length))

(in-package #:almighty-html/parser/input-stream)

(defclass html-input-stream ()
  ((data :initarg :data
     :accessor html-input-stream-data
     :initform ""
     :type string)
   (pos :initarg :pos
        :accessor html-input-stream-pos
        :initform 0
        :type fixnum)
   (length :initarg :length
           :accessor html-input-stream-length
           :initform 0
           :type fixnum))
  (:documentation "Preprocessed character input stream for the HTML tokenizer."))

(defun make-html-input-stream (string)
  "Create an input stream, performing preprocessing (CR normalization)."
  (let ((preprocessed (preprocess-input string)))
    (make-instance 'html-input-stream
                   :data preprocessed
                   :pos 0
                   :length (length preprocessed))))

(defun preprocess-input (string)
  "Normalize newlines: CR LF → LF, standalone CR → LF.
Per WHATWG §13.2.3.5."
  (with-output-to-string (out)
    (loop with len = (length string)
          with i = 0
          while (< i len)
          for ch = (char string i)
          do (cond
               ((char= ch #\Return)
                (write-char #\Newline out)
                ;; Skip following LF if present
                (when (and (< (1+ i) len)
                           (char= (char string (1+ i)) #\Newline))
                  (incf i)))
               (t (write-char ch out)))
             (incf i))))

(defun stream-next-char (stream)
  "Consume and return the next character, or NIL at EOF."
  (with-slots (data pos length) stream
    (if (>= pos length)
        nil
        (prog1 (char data pos)
          (incf pos)))))

(defun stream-peek-char (stream)
  "Peek at the next character without consuming it, or NIL at EOF."
  (with-slots (data pos length) stream
    (if (>= pos length)
        nil
        (char data pos))))

(defun stream-reconsume (stream)
  "Push back the last consumed character (unconsume)."
  (with-slots (pos) stream
    (when (> pos 0)
      (decf pos))))

(defun stream-eof-p (stream)
  "Return T if the stream is at EOF."
  (>= (html-input-stream-pos stream) (html-input-stream-length stream)))

(defun stream-match-insensitive (stream target)
  "Peek ahead and check if the next characters case-insensitively match TARGET.
Does not consume. Returns T/NIL."
  (with-slots (data pos length) stream
    (let ((tlen (length target)))
      (when (<= (+ pos tlen) length)
        (loop for i from 0 below tlen
              always (char-equal (char data (+ pos i))
                                 (char target i)))))))

(defun stream-consume-chars (stream n)
  "Consume N characters and return them as a string."
  (with-slots (data pos length) stream
    (let ((end (min (+ pos n) length)))
      (prog1 (subseq data pos end)
        (setf pos end)))))
