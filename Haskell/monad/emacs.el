(defun empty? (x)
  "True if length of X is zero."
  (= 0 (length x)))


;;;;;;;;;;;;;;;;;;;;
;    page 1
;;;;;;;;;;;;;;;;;;;;


(defun str-head (str)
  (substring str 0 1))
(defun str-tail (str)
  (substring str 1))
(defun pair (a b)
  (cons a b))
(defun parsed-value (pair)
  (car pair))
(defun parsed-leftover (pair)
  (cdr pair))


(defun parse-a (input)
  "A very simple parser - parses 'a' or nothing."
  (unless (empty? input)
	(if (string= "a" (str-head input))
		(pair :found-a (str-tail input))
	  nil)))

(parse-a "abracadabra")
(parse-a "dogs of war")

;;;;;;;;;;;;;;;;;;;;
;    page 2
;;;;;;;;;;;;;;;;;;;;

