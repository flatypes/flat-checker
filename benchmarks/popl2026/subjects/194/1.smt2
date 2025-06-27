; Input: /benchmark/subjects/194.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.diff re.allchar _let_1))) (str.in_re s (re.++ (re.++ ((_ re.^ 0) _let_2) (re.* _let_2)) _let_1)))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (not (and (>= _let_2 0) (< _let_2 _let_1))))))
(check-sat)
(exit)