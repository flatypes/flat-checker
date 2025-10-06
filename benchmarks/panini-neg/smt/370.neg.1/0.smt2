; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/370.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.range "a" "b"))) (str.in_re s (re.union (re.++ _let_1 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (not (or (= s "a") (= s "b"))))
(check-sat)
(exit)