; Input: /benchmark/subjects/527.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.++ _let_1 _let_1) (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar))))))
(assert (let ((_let_1 (str.substr s 0 (- 2 0)))) (not (= (str.at _let_1 0) (str.at _let_1 1)))))
(check-sat)
(exit)