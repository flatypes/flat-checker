; Input: /benchmark/subjects/140.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.diff re.allchar _let_1))) (str.in_re s (re.++ (re.++ (re.++ ((_ re.^ 0) _let_2) (re.* _let_2)) _let_1) (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))))
(assert (not (str.contains s "a")))
(check-sat)
(exit)