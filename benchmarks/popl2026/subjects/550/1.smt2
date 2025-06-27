; Input: /benchmark/subjects/550.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (str.in_re s ((_ re.loop 0 1) (re.++ (re.union (re.diff re.allchar _let_1) (re.++ _let_1 re.allchar)) (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))))))
(assert (not (or (or (= s "a") (distinct s "b")) (= s "c"))))
(check-sat)
(exit)