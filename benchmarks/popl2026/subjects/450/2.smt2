; Input: /benchmark/subjects/450.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (str.to_re "a"))) (let ((_let_3 (re.++ (re.++ _let_2 (re.diff re.allchar (re.union _let_2 _let_1))) _let_1))) (str.in_re s (re.++ ((_ re.^ 0) _let_3) (re.* _let_3)))))))
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (str.to_re "a"))) (let ((_let_3 (re.++ (re.++ _let_2 (re.diff re.allchar (re.union _let_2 _let_1))) _let_1))) (not (str.in_re (str.substr s 0 (- (str.len s) 0)) (re.++ ((_ re.^ 0) _let_3) (re.* _let_3))))))))
(check-sat)
(exit)