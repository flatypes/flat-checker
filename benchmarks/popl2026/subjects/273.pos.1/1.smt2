; Input: /benchmark/subjects/273.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.opt (str.to_re "b")))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (not (and (>= _let_2 0) (< _let_2 _let_1))))))
(check-sat)
(exit)