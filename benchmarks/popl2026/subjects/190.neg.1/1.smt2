; Input: /benchmark/subjects/190.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.* (re.union (re.diff re.allchar _let_1) (re.++ (re.++ _let_1 re.allchar) (re.* re.allchar)))))))
(assert (not (str.contains s "a")))
(check-sat)
(exit)