; Input: /benchmark/subjects/363.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (re.* re.allchar)) (str.to_re "b"))))
(assert (let ((_let_1 (str.len s))) (not (and (>= (- _let_1 1) 0) (>= _let_1 0)))))
(check-sat)
(exit)