; Input: /benchmark/subjects/363.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.union (re.++ _let_1 (re.opt (re.++ (re.* re.allchar) (re.diff re.allchar (str.to_re "b"))))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (>= (- (str.len s) 1) 0))
(assert (>= (str.len s) 0))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (not (= (+ (str.indexof (str.substr s _let_2 (- _let_1 _let_2)) "b" 0) _let_2) _let_2)))))
(check-sat)
(exit)