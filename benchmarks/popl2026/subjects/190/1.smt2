; Input: /benchmark/subjects/190.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.* (re.diff re.allchar _let_1)) _let_1))))
(assert (not (str.contains s "a")))
(check-sat)
(exit)