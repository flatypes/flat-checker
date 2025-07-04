; Input: /benchmark/subjects/065.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ re.allchar (re.opt (re.++ re.allchar (re.opt (re.++ (re.++ re.allchar re.allchar) (re.* re.allchar)))))))))
(assert (let ((_let_1 (- (str.len s) 1))) (not (and (<= _let_1 _let_1) (>= _let_1 (- 0 1))))))
(check-sat)
(exit)