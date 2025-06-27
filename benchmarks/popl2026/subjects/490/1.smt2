; Input: /benchmark/subjects/490.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "1"))) (let ((_let_2 (str.to_re "0"))) (let ((_let_3 (re.union _let_2 _let_1))) (str.in_re s (re.++ (re.++ (re.++ (re.++ ((_ re.^ 0) _let_3) (re.* _let_3)) _let_2) _let_1) _let_1))))))
(assert (not (and (>= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)