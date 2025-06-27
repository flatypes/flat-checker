; Input: /benchmark/subjects/182.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.diff re.allchar _let_1) _let_1))))
(assert (distinct (str.at s 0) "a"))
(assert (distinct (str.at s 1) "a"))
(assert (not false))
(check-sat)
(exit)