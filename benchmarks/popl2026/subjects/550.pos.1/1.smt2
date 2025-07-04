; Input: /benchmark/subjects/550.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (str.in_re s (re.opt (re.++ (re.union (re.diff re.allchar _let_1) (re.++ _let_1 re.allchar)) (re.* re.allchar))))))
(assert (not (or (or (= s "a") (distinct s "b")) (= s "c"))))
(check-sat)
(exit)