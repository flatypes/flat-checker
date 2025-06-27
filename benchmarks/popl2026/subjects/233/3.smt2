; Input: /benchmark/subjects/233.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (str.in_re s (re.++ (str.to_re "a") (re.++ ((_ re.^ 0) _let_1) (re.* _let_1))))))
(assert (let ((_let_1 (+ 0 1))) (not (and (<= 1 _let_1) (<= _let_1 (str.len s))))))
(check-sat)
(exit)