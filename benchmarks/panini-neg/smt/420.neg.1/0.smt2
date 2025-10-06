; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/420.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.range "a" "c"))) (str.in_re s (re.union (re.++ _let_1 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (not (or (or (= s "a") (= s "b")) (= s "c"))))
(check-sat)
(exit)