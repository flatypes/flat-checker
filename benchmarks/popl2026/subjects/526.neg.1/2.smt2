; Input: /benchmark/subjects/526.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.opt _let_1) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (not (= (str.len (str.substr s 0 (- 2 0))) 2)))
(check-sat)
(exit)