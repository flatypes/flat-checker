; Input: /benchmark/subjects/311.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (re.* re.allchar) (str.to_re "a")) (str.to_re "b"))))
(assert (let ((_let_1 (str.len s))) (not (and (>= (- _let_1 2) 0) (>= _let_1 0)))))
(check-sat)
(exit)