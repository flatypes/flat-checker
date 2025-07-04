; Input: /benchmark/subjects/523.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.++ _let_1 _let_1) (re.* re.allchar)))))
(assert (>= 0 0))
(assert (>= 2 0))
(assert (not (= (str.substr s 0 (- 2 0)) "aa")))
(check-sat)
(exit)