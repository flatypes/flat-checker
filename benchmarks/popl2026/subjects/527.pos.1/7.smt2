; Input: /benchmark/subjects/527.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.++ _let_1 _let_1) (re.* re.allchar)))))
(assert (>= 0 0))
(assert (< 0 (str.len s)))
(assert (not (= (str.at s 0) "a")))
(check-sat)
(exit)