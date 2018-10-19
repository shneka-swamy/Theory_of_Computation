(defun set-member (set item)
  (cond ((not set) nil)
	((eq (car set) item) t)
	(t (set-member (cdr set) item))))

(defun set-union (set-1 set-2)
  (cond ((not set-1) set-2)
	((not (set-member set-2 (car set-1))) (cons (car set-1) (set-union (cdr set-1) set-2)))
	(t (set-union (cdr set-1) set-2))))

(defun check_loop(states)

  ;; Can use only certain functions inside the for loop others are implemented using a do function
  ;; (loop for x in '(1 2 3 4)
  ;;    do  (print x)))
  
   ;; Use of Hash table in lisp
  (let ((hash (make-hash-table)))
  (loop for i in states
     do (setf (gethash i  hash) (list i)))

  (loop for k in states
     do  (if (gethash k hash)
	     (setf (gethash k hash) (set-union (gethash k hash) (list "q3")))))
  (loop for j in states
     do (print j)
	(print (gethash j hash)))))
  
