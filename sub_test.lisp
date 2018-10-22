(defun e-closure (states edges hash)
  (loop for i in states
     do (setf (gethash i hash) (list i)))

  (loop for j in edges
     do (if (and (equal (second j) :epsilon) (gethash (car j) hash))  
	    (setf (gethash (car j) hash) (set-union (gethash (car j) hash) (list (third j))))))

  ;; K is included for verification (to make sure all subsets are considered for the backward brackets)
  (loop for k in (list 1 2) 
     do(loop for l in states
	  do (loop for m in (gethash l hash)
		do (if (not (equal l m))
		       (setf (gethash l hash) (set-union (gethash l hash) (gethash m hash))))))))

(defun set-equivalence (set-0 set-1 output)
   (loop while (and (equal (list-length set-0) (list-length set-1)) (not (equal (list-length set-0) 0)) (not (equal (list-length set-1) 0)))
      do (setq value (car set-0))
      do (setf set-0 (remove value set-0))
      do (setf set-1 (remove value set-1)))

   (if (and (equal (list-length set-0) 0) (equal (list-length set-1) 0))
   
      (setq output T))
  (print output))
  

(defun nfa->dfa (nfa)
  (setf edges (finite-automaton-edges nfa))
  
  (let ((hash (make-hash-table)))
    (e-closure (finite-automaton-states nfa) (finite-automaton-edges nfa) hash)

    (setf (finite-automaton-alphabet nfa) (remove :epsilon (finite-automaton-alphabet nfa)))
    (print (finite-automaton-alphabet nfa))
    (loop for edge in edges
       do(if (equal (second edge) :epsilon)
	     (setf edges (remove edge edges :test #'equal))))
    (print edges) 
    (loop for k in (finite-automaton-states nfa)
       do (print k)
	  (print (gethash k hash)))
    )
)
