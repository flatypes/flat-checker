; Input: /benchmark/subjects/108.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ _let_1 _let_1))))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)