; Input: /benchmark/subjects/980_parser.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.union _let_1 (re.++ (re.++ (re.diff re.allchar _let_1) (str.to_re "b")) (re.* re.allchar))))))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)