; Input: /benchmark/subjects/970_getAddrSpec.py
(set-logic ALL)
(declare-const email String)
(assert (let ((_let_1 (str.to_re ">"))) (let ((_let_2 (str.to_re "<"))) (str.in_re email (re.++ (re.++ (re.++ (re.++ (re.* (re.diff re.allchar (re.union _let_2 _let_1))) _let_2) (re.* (re.diff re.allchar _let_1))) _let_1) (re.* re.allchar))))))
(assert (let ((_let_1 (+ (str.indexof email "<" 0) 1))) (not (and (>= _let_1 0) (>= (+ (str.indexof (str.substr email _let_1 (- (str.len email) _let_1)) ">" 0) _let_1) 0)))))
(check-sat)
(exit)