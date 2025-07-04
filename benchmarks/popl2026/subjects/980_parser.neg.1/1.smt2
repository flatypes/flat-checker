; Input: /benchmark/subjects/980_parser.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.opt (re.union (re.++ (re.diff re.allchar _let_1) (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2)))) (re.++ (re.++ _let_1 re.allchar) (re.* re.allchar))))))))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)