; Input: /benchmark/subjects/044.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar re.allchar)))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 2))) (not (and (>= _let_2 0) (< _let_2 _let_1))))))
(check-sat)
(exit)