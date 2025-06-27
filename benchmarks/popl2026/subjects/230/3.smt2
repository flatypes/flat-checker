; Input: /benchmark/subjects/230.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (str.in_re s (re.++ (str.to_re "a") (re.++ ((_ re.^ 0) _let_1) (re.* _let_1))))))
(assert (not (and (>= 1 1) (<= 1 (str.len s)))))
(check-sat)
(exit)