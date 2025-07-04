; Input: /benchmark/subjects/141.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.++ (re.* (re.diff re.allchar _let_1)) _let_1) (re.* re.allchar)))))
(assert (not (>= (str.indexof s "a" 0) 0)))
(check-sat)
(exit)