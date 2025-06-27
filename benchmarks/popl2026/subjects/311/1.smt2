; Input: /benchmark/subjects/311.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)) (str.to_re "a")) (str.to_re "b"))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 2))) (not (= (str.substr s _let_2 (- _let_1 _let_2)) "ab")))))
(check-sat)
(exit)