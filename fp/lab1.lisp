(defun get-priority (operator)
  ;; Check operator type and return priority
  (cond
    ;; Addition and subtraction have priority 1
    ((or (equal operator '+) (equal operator '-)) 1)
    ;; Multiplication and division have priority 2 
    ((or (equal operator '*) (equal operator '/)) 2)
    ;; All other symbols have priority 0
    (t 0)
  )
)

(defun is-operator (symbol)
  ;; Check if symbol is in list of arithmetic operators
  (member symbol '(+ - * /)
  )
)

(defun find-lowest-priority-op (expr &optional pos min-pos min-priority)
  ;; Base case: empty expression returns minimum position found
  (cond 
    ((null expr) min-pos)
    ;; Recursive case: check current element
    (t (let* (
              ;; Get first element of expression
              (current (car expr))
              ;; Calculate priority of current element
              (priority (if (is-operator current)
                          (get-priority current)
                          most-positive-fixnum)
              )
         )
         ;; If current priority is lower or equal, update minimum
         (if (<= priority min-priority)
             (find-lowest-priority-op 
                (cdr expr)      ; Rest of expression
                (1+ pos)        ; Increment position
                pos            ; New minimum position
                priority       ; New minimum priority
             )
             ;; Otherwise keep searching with same minimum
             (find-lowest-priority-op 
                (cdr expr)      ; Rest of expression
                (1+ pos)        ; Increment position
                min-pos        ; Keep same minimum position
                min-priority   ; Keep same minimum priority
             )
         )
      )
    )
  )
)

(defun get-element (list n)
  ;; Return nil for empty list
  (cond ((null list) nil)
        ;; Return first element when n = 0
        ((= n 0) (car list))
        ;; Recursively decrement n and move through list
        (t (get-element (cdr list) (- n 1)))
  )
)

(defun take-elements (list n)
  ;; Take first n elements from list
  (cond ((or (null list) (= n 0)) nil)
        (t (cons (car list) 
                (take-elements (cdr list) (- n 1))))
  )
)

(defun drop-elements (list n)
  ;; Drop first n elements from list
  (cond ((or (null list) (= n 0)) list)
        (t (drop-elements (cdr list) (- n 1)))
  )
)

(defun combine-lists (list1 list2)
  ;; Combine two lists
  (cond ((null list1) list2)
        (t (cons (car list1) 
                (combine-lists (cdr list1) list2)))
  )
)

(defun to-prefix (expr)
  ;; Base case: empty or single element list
  (cond ((or (null expr) (null (cdr expr)))
         expr)
        ;; Convert infix to prefix
        (t (let ((index (find-lowest-priority-op expr)))
             ;; If no operator found, return unchanged
             (if (= index -1)
                 expr
                 ;; Split around operator and recurse
                 (let ((operator (get-element expr index))
                       (left-part (take-elements expr index))
                       (right-part (drop-elements expr (1+ index))))
                   ;; Construct prefix expression
                   (cons operator 
                         (combine-lists 
                            (to-prefix left-part)
                            (to-prefix right-part))
                   )
                 )
             )
        )
  )
)

;; Test get-priority
(format t "~%Testing get-priority:~%")
(format t "Priority of +: ~a (expected 1)~%" (get-priority '+))
(format t "Priority of -: ~a (expected 1)~%" (get-priority '-))
(format t "Priority of *: ~a (expected 2)~%" (get-priority '*))
(format t "Priority of /: ~a (expected 2)~%" (get-priority '/))
(format t "Priority of $: ~a (expected 0)~%" (get-priority '$))

;; Test is-operator
(format t "~%Testing is-operator:~%")
(format t "+ is operator: ~a (expected T)~%" (is-operator '+))
(format t "* is operator: ~a (expected T)~%" (is-operator '*))
(format t "2 is operator: ~a (expected NIL)~%" (is-operator '2))

;; Test find-lowest-priority-op
(format t "~%Testing find-lowest-priority-op:~%")
(format t "2 + 3 * 4: ~a (expected 1)~%" 
        (find-lowest-priority-op '(2 + 3 * 4)))
(format t "2 * 3 + 4: ~a (expected 3)~%" 
        (find-lowest-priority-op '(2 * 3 + 4)))
(format t "Empty list: ~a (expected -1)~%" 
        (find-lowest-priority-op nil))
(format t "No operators: ~a (expected -1)~%" 
        (find-lowest-priority-op '(1 2 3)))

;; Test to-prefix
(format t "~%Testing to-prefix:~%")
(format t "2 + 3 * 4: ~a~%" (to-prefix '(2 + 3 * 4)))
(format t "2 * 3 + 4: ~a~%" (to-prefix '(2 * 3 + 4)))
(format t "Single number: ~a~%" (to-prefix '(5)))
(format t "Empty list: ~a~%" (to-prefix nil))
