; Input: /benchmark/subjects/392.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.union (str.to_re "a") (str.to_re "b")))))
(assert (let ((_let_1 (str.len s))) (not (or (= _let_1 0) (and (= _let_1 1) (or (>= (str.indexof s "a" 0) 0) (>= (str.indexof s "b" 0) 0)))))))
(check-sat)
(exit)