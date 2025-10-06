; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/270.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.++ (re.union (re.diff re.allchar _let_2) (re.++ _let_2 re.allchar)) (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (not (or (= s "a") (= s "ab"))))
(check-sat)
(exit)