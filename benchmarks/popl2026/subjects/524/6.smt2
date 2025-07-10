; Input: /benchmark/subjects/524.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.++ _let_1 _let_1) (re.* re.allchar)))))
(assert (not (= (str.at (str.substr s 0 (- 2 0)) 1) "a")))
(check-sat)
(exit)