; Input: /benchmark/subjects/360.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar))) (str.to_re "b"))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (not (and (>= _let_2 0) (< _let_2 _let_1))))))
(check-sat)
(exit)