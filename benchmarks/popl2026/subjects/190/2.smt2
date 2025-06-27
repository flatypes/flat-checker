; Input: /benchmark/subjects/190.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.diff re.allchar _let_1))) (str.in_re s (re.++ (re.++ ((_ re.^ 0) _let_2) (re.* _let_2)) _let_1)))))
(assert (not (= (str.indexof s "a" 0) (- (str.len s) 1))))
(check-sat)
(exit)