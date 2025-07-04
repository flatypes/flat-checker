; Input: /benchmark/subjects/363.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.union (re.++ _let_1 (re.opt (re.++ (re.* re.allchar) (re.diff re.allchar (str.to_re "b"))))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (let ((_let_1 (str.len s))) (not (and (>= (- _let_1 1) 0) (>= _let_1 0)))))
(check-sat)
(exit)