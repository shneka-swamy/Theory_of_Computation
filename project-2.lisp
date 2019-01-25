;;;;;;;;;;;;;;;;;;;;;;;;
;;; HELPER FUNCTIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun var-p (e)
  "Is expression a variable?"
  (atom e))

(defun lit-p (e)
  "Is expression a literal?"
  (or (var-p e)
      (and (eq (car e) 'not)
           (var-p (second e))
           (null (cddr e)))))

(defun and-p (e)
  "Is expression an AND?"
  (and (consp e)
       (eq (car e) 'and)))

(defun or-p (e)
  "Is expression an OR?"
  (and (consp e)
       (eq (car e) 'or)))

(defun not-p (e)
  "Is expression an NOT?"
  (and (consp e)
       (eq (car e) 'not)))


(defun boolean-implies (a b)
  (or (not a) b))

(defun boolean-iff (a b)
  (and (boolean-implies a b)
       (boolean-implies b a)))

(defun boolean-xor (a b)
  (if a
      (not b)
      b))

(defun exp-eval (e bindings)
  "Evalate expression given bindings."
  (labels ((rec (e)
             (cond
               ((eq t e)
                t)
               ((null e)
                nil)
               ((var-p e)
                (let ((assoc (assoc e bindings)))
                  (if assoc
                      (cdr assoc)
                      (error "No binding for ~A" e))))
               (t
                (destructuring-bind (op &rest args) e
                  (ecase op
                    (not
                     (assert (null (cdr args)))
                     (not (rec (car args))))
                    (and (every #'rec args))
                    (or (some #'rec args))
                    (:xor
                     (assert (= 2 (length args)))
                     (boolean-xor (rec (first args))
                                  (rec (second args))))
                    (:implies
                     (assert (= 2 (length args)))
                     (boolean-implies (rec (first args))
                                      (rec (second args))))
                    (:iff
                     (assert (= 2 (length args)))
                     (boolean-iff (rec (first args))
                                  (rec (second args))))))))))
    (rec e)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONVERSION TO CONJUNCTIVE NORMAL FORM ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nnf-p (e)
  "Is expression in negation normal form?"
  (labels ((visit (e)
             (or (var-p e)
                 (destructuring-bind (op &rest args) e
                   (case op
                     ((and or)
                      (every #'visit args))
                     (not (and (var-p (car args))
                               (null (cdr args))))
                     (otherwise nil))))))
    (visit e)))


(defun cnf-p (e)
  "Is expression in conjunctive normal form?"
  (labels ((dis-p (e)
             (and (consp e)
                  (eq 'or (car e))
                  (every #'lit-p (cdr e)))))
    (and (eq 'and (car e))
         (every #'dis-p (cdr e)))))


(defun exp-op (op &rest args)
  "Construct in n-ary s-expression"
  (labels ((visit (args e)
             (if (and (consp e)
                      (eq (car e) op))
                 (reduce #'visit (cdr e) :initial-value args)
                 (cons e args))))
    (cons op
          (reduce #'visit args :initial-value nil))))



;; Convert boolean formula to negation-normal form:
;; * Only AND, OR, and NOT operators
;; * NOT is only applied to individual variables
;;
;;
;; Examples:
;; ---------
;;
;; (exp->nnf '(not (not a))) => a
;;
;; (exp->nnf '(not (and a b))) => (OR (NOT A) (NOT B))
;;
;; (exp->nnf '(not (or a (not b)))) => (AND (NOT A) B)
;;
;; (exp->nnf '(not (:iff a b))) =>
;;               (OR (AND A (NOT B)) (AND B (NOT A)))
;;

(defun exp->nnf (e)
  "Convert an expression to negation normal form."
  (labels ((base (e truth)
             (if truth
                 e
                 `(not ,e)))
           (visit (e truth)
             (if (var-p e)
                 (base e truth)
                 (destructuring-bind (op &rest args) e
                   (case op
                     (:implies
                      (destructuring-bind (a b) args
                        ;; TODO: handle this case
                        e))
                     (:iff
                      (destructuring-bind (a b) args
                        ;; TODO: handle this case
                        e))
                     (not
                      (assert (and args (null (cdr args))))
                      (visit (car args) (not truth)))
                     (and
                      ;; TODO: handle this case
                      e)
                     (or
                      ;; TODO: handle this case
                       )
                     (otherwise
                      (base e truth)))))))

    (visit e t)))


;; Distribution examples:
;; ---------------------
;;
;; (or a (and b c))
;;     => (and (or a b) (or a c))
;;
;; (or x (and a b c))
;;     => (and (or x a) (or x b) (or x c))
;;
;; (or (or x y) (and b c))
;;      => (and (or x y b) (or x y c))
;;
;; (or (and x y) (and b c))
;;     => (and (or (and x y) b) (or (and x y) c))
;;     => (and (and (or b x) (or b y))
;;             (and (or c x) (or c y)))
;;     => (and (or b x) (or b y) (or c x) (or c y))
;;
;;
;; (or (and x y z) (and a b c))
;;     => (and (or x (and a b c))
;;             (or y (and a b c))
;;             (or z (and a b c)))
;;     => (and (and (or x a) (or x b) (or x c))
;;             (and (or y a) (or y b) (or y c))
;;             (and (or z a) (or z b) (or z c)))
;;     => (and (or x a) (or x b) (or x c)
;;             (or y a) (or y b) (or y c)
;;             (or z a) (or z b) (or z c))



;; Distribute literals over a single AND expression:
;;
;; (or lit-0 ... lit-1
;;     (and (or ...) (or ...) ...))
;;
;; The result should be in conjunctive normal form
(defun %dist-or-and-1 (literals and-exp)
  (assert (every #'lit-p literals))
  (assert (cnf-p and-exp))
  ;; TODO: implement
  `(or ,@literals ,and-exp))

;; Distribute OR over two AND expressions:
;;
;; (or (and (or ...) (or ...) ...)
;;     (and (or ...) (or ...) ...))
;;
;; The result should be in conjunctive normal form
(defun %dist-or-and-and (and-exp-1 and-exp-2)
  (assert (cnf-p and-exp-1))
  (assert (cnf-p and-exp-2))
  ;; TODO: implement
  `(or ,and-exp-1 ,and-exp-2))


;; Distribute n-ary OR over the AND arguments:
;;
;; (or lit-0 ... lit-1
;;     (and (or ...) (or ...) ...)
;;     ...
;;     (and (or ...) (or ...) ...))
;;
;; The result is in CNF
(defun %dist-or-and-n (literals and-exps)
  (assert (every #'lit-p literals))
  (assert (every #'cnf-p and-exps))
  (if literals
      (if and-exps
          (reduce #'%dist-or-and-and
                  (cdr and-exps)
                  :initial-value (%dist-or-and-1 literals (car and-exps)))
          `(and ,(apply #'exp-op 'or literals)))
      (reduce #'%dist-or-and-and
               and-exps)))

(defun cnf-or (e)
  (assert (or-p e))
  (let ((literals)
        (and-args))
    (labels ((visit (e)
               (cond
                 ((lit-p e)
                  (push e literals))
                 ((or-p e)
                  (map nil #'visit (cdr e)))
                 ((and-p e)
                  (let ((e (cnf-and e)))
                    (assert (cnf-p e))
                    (push e and-args))))))
      (map nil #'visit (cdr e)))
    (%dist-or-and-n literals and-args)))

(defun cnf-and (e)
  (assert (and-p e))
  (labels ((visit (args e)
             (cond
               ((lit-p e)
                (cons `(or ,e) args))
               ((or-p e)
                (let ((e (cnf-or e)))
                  (assert (cnf-p e))
                  (append (cdr e)
                          args)))
               ((and-p e)
                (reduce #'visit (cdr e)
                        :initial-value args)))))
    (cons 'and (visit nil e))))


(defun nnf->cnf (e)
  (assert (nnf-p e))
  (cond ((lit-p e) `(and (or ,e)))
        ((and-p e) (cnf-and e))
        ((or-p e) (cnf-or e))
        (t (error "Invalid expression: ~A" e))))

(defun exp->cnf (e)
  "Convert an expression to conjunctive normal form."
  (nnf->cnf (exp->nnf e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAVIS-PUTNAM-LOGEMANN-LOVELAND (DPLL) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct maxterm
  "An OR of literals"
  pos
  neg)


;;; For a list of MAXTERMS:
;;; - NIL is (AND) => true
;;; - :UNSAT indicates a false clause in the list

(defun cnf-maxterms (e)
  "Construct list of maxterms for a CNF formula"
  (assert (cnf-p e))
  (reduce (lambda (terms e)
            (assert (or-p e))
            (let ((pos) (neg))
              (dolist (e (cdr e))
                (assert (lit-p e))
                (if (not-p e)
                    (push (second e) neg)
                    (push e pos)))
              (if (intersection pos neg)
                  terms
                  (cons (make-maxterm :pos pos
                                      :neg neg)
                        terms))))
          (cdr e) :initial-value nil))

(defun maxterm-remove (pos neg)
  (when (or pos neg)
    (make-maxterm :pos pos
                  :neg neg)))

(defun maxterm-bind (maxterm var value)
  (labels ((in-pos ()
             (find var (maxterm-pos maxterm)
                   :test #'equal))
           (in-neg ()
             (find var (maxterm-neg maxterm)
                   :test #'equal)))
  (if value
      (cond
        ((in-pos) t)
        ((in-neg)
         (maxterm-remove (maxterm-pos maxterm)
                         (remove var (maxterm-neg maxterm) :test #'equal)))
        (t maxterm))
      ;; value = nil
      (cond
        ((in-neg) t)
        ((in-pos)
         (maxterm-remove (remove var (maxterm-pos maxterm) :test #'equal)
                         (maxterm-neg maxterm)))
        (t maxterm)))))

(defun maxterm-unit-p (maxterm)
  (let ((pos (maxterm-pos maxterm))
        (neg (maxterm-neg maxterm)))
    (or (and (null pos)
             neg (null (cdr neg)))
        (and (null neg)
             pos (null (cdr pos))))))

(defun dpll-bind (maxterms var value bindings)
  "Bind VALUE to VAR in the given MAXTERMS expression.

RESULT: (VALUES MAXTERMS BINDINGS)"
  (let ((bindings (cons (cons var value) bindings)))
    (labels ((rec (new-terms rest)
               (if rest
                   (destructuring-bind (x &rest rest) rest
                     (let ((new-term (maxterm-bind x var value)))
                       (cond
                         ;; false clause
                         ((null new-term)
                          (values :UNSAT nil))
                         ;; true clause
                         ((eq t new-term)
                          (rec new-terms rest))
                         ;; still have variables
                         (t
                          (rec (cons new-term new-terms)
                               rest)))))
                   ;; end of terms
                   (values new-terms bindings))))
    (rec nil maxterms))))

(defun dpll-unit-propagate (maxterms bindings)
  "Perform unit propagation.

RESULT: (VALUES MAXTERMS BINDINGS)"
  (labels ((repeat (var value)
             ;; Bind literal from the unit clause and re-propagate
             (multiple-value-call #'dpll-unit-propagate
               (dpll-bind maxterms var value bindings)))
           (rec (rest)
             (if (consp rest)
                 (destructuring-bind (x &rest rest) rest
                   (if (maxterm-unit-p x)
                       (progn
                         ;; TODO: propagate the unit clause
                         nil)
                       (rec rest)))
                 ;; no unit clauses
                 (values maxterms bindings))))
    (rec maxterms)))


(defun dpll-choose-literal (maxterms)
  "Very simple implementation to choose a branching literal."
  (let* ((term (car maxterms))
         (pos (maxterm-pos term))
         (neg (maxterm-neg term)))
    (cond (pos (car pos))
          (neg (car neg))
          (t (error "Bad maxterm.")))))

;; Return T if the formula is SAT
;; Return NIL if the formula is UNSAT
(defun dpll (maxterms)
  (labels ((maxterm-result (maxterms)
             (or (null maxterms)       ; all maxterms true => (AND)
                 (eq :UNSAT maxterms))) ; found a false maxterm clauses
           (rec (maxterms bindings)
             (if (maxterm-result maxterms)
                 ;; already have a result
                 (values maxterms bindings)
                 ;; unit propagate
                 (progn
                   ;; TODO: implement the recursive case
                   nil))))
    (multiple-value-bind (nil-or-unsat bindings)
        (rec maxterms nil)
      (cond
        ((null nil-or-unsat)
         (values t bindings))
        ((eq :UNSAT nil-or-unsat)
         (values nil bindings))
        (t (error "Invalid final result ~A" nil-or-unsat))))))


(defun sat-p (e)
  "Check satisfiability of e."
  (multiple-value-bind (is-sat bindings)
      (dpll (cnf-maxterms (exp->cnf e)))
    (when is-sat
      ;; Check that expression evaluates to true with chosen bindings
      (assert (exp-eval e bindings)))
    (values is-sat bindings)))
