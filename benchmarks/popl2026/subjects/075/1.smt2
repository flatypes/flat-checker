; Input: /benchmark/subjects/075.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) (re.* re.allchar))))
(assert (let ((_let_1 (str.len s))) (not (and (> _let_1 0) (> _let_1 1)))))
(check-sat)
(exit)