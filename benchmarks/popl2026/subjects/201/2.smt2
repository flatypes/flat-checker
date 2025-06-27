; Input: /benchmark/subjects/201.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.++ ((_ re.^ 0) _let_1) (re.* _let_1)) (re.diff re.allchar _let_1)))))
(assert (not (distinct (str.at s (- (str.len s) 1)) "a")))
(check-sat)
(exit)