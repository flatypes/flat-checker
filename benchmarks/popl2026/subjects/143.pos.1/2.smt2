; Input: /benchmark/subjects/143.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.* re.allchar))) (str.in_re s (re.++ (re.++ _let_1 (str.to_re "a")) _let_1))))
(assert (let ((_let_1 (str.len s))) (not (not (str.contains (str.substr s _let_1 (- _let_1 _let_1)) "a")))))
(check-sat)
(exit)