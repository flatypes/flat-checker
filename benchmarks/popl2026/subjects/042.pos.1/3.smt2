; Input: /benchmark/subjects/042.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar re.allchar)))
(assert (<= (str.len s) 2))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 2))) (not (and (>= _let_2 0) (< _let_2 _let_1))))))
(check-sat)
(exit)