; Input: /benchmark/subjects/471.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "0"))) (let ((_let_2 (re.union _let_1 (re.++ _let_1 (str.to_re "1"))))) (str.in_re s (re.++ ((_ re.^ 0) _let_2) (re.* _let_2))))))
(assert (distinct s ""))
(assert (not (= (str.at s 0) "0")))
(check-sat)
(exit)