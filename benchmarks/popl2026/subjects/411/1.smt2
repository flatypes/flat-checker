; Input: /benchmark/subjects/411.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.union (str.to_re "a") (str.to_re "b")))) (str.in_re s (re.++ ((_ re.^ 0) _let_1) (re.* _let_1)))))
(assert (not (and (<= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)