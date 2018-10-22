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

(defun set-equivalence (set-0 set-1)
   (loop while (and (equal (list-length set-0) (list-length set-1)) (not (equal (list-length set-0) 0)) (not (equal (list-length set-1) 0)))
      do (setq value (car set-0))
      do (setf set-0 (remove value set-0))
      do (setf set-1 (remove value set-1)))

   (cond ((and (equal (list-length set-0) 0) (equal (list-length set-1) 0)) T)
         (t NIL)))
 ; (print output))
  

(defun nfa->dfa (nfa)
  (setq edges (finite-automaton-edges nfa))
  (setq states (finite-automaton-states nfa))
  (setq alp (finite-automaton-alphabet nfa))
  (setq start (finite-automaton-start nfa))
  (setq accept (finite-automaton-accept nfa))
  
  (let ((hash (make-hash-table)))
   
    (e-closure states edges hash)

    (setf alp (remove :epsilon alp))
    (print alp)
    (loop for edge in edges
       do(if (equal (second edge) :epsilon)
	     (setf edges (remove edge edges :test #'equal))))
    (print edges)

    (setq stack (list (gethash start hash)))
    (setq stack_perm (list (gethash start hash)))
    (setq new_edges (list '()))
    (print stack)

    (setq var '())

    (loop while (not (equal (list-length stack) 0))
       do (setq stack_temp (car stack))
       do (setq stack (cdr stack))
       do (loop for alpha in alp
	     do(setq flag 0)
	     do(setq output NIL)
	     do(loop for edge in edges
		  do(if (and (equal alpha (second edge)) (set-member stack_temp (car edge)))
			(setq var (set-union var (gethash (third edge) hash)))))
	       
	     do(loop for value in stack_perm
		  do (if (set-equivalence value var)
			  (setq flag (+ flag 1))))
	     do(if(equal flag 0)
		  (progn 
		   (setq stack (cons var stack))
		   (setq stack_perm(cons var stack_perm))))
	    ; do(print  stack_perm)
	    ; do(print stack_temp)
	    ; do(print var)
	    ; do(print (position stack_temp stack_perm :test #'equal))
	    ; do(print (position var stack_perm :test #'equal))
	     do(setq index-1 (- (list-length stack_perm) (position stack_temp stack_perm :test #'equal)))
	     do(setq index-2 (- (list-length stack_perm)  (position var stack_perm :test #'equal)))
	     do(setq new_edges (cons (list index-1  alpha index-2) new_edges))
	       (setq var '())))
		  
    (print stack_perm)
    (print new_edges)
	 
    ;(loop for k in states
     ;  do (print k)
;	  (print (gethash k hash)))
    )
)
