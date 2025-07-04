; Input: /benchmark/subjects/526.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.opt _let_1) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (let ((_let_1 (str.substr s 0 (- 2 0)))) (not (= (str.at _let_1 0) (str.at _let_1 1)))))
(check-sat)
(exit)