; Input: /benchmark/subjects/203.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.++ ((_ re.^ 0) _let_1) (re.* _let_1)) (re.diff re.allchar _let_1)))))
(assert (not (and (>= 0 0) (<= 0 (str.len (str.substr s 0 (- (- (str.len s) 1) 0)))))))
(check-sat)
(exit)