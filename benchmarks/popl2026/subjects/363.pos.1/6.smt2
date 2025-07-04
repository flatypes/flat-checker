; Input: /benchmark/subjects/363.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (re.* re.allchar)) (str.to_re "b"))))
(assert (>= (- (str.len s) 1) 0))
(assert (>= (str.len s) 0))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (not (= (+ (str.indexof (str.substr s _let_2 (- _let_1 _let_2)) "b" 0) _let_2) _let_2)))))
(check-sat)
(exit)