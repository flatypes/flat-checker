; Input: /benchmark/subjects/401.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (re.* (str.to_re "a")) (re.* (str.to_re "b")))))
(assert (let ((_let_1 (= 0 0))) (not (ite (str.contains s "a") _let_1 _let_1))))
(check-sat)
(exit)