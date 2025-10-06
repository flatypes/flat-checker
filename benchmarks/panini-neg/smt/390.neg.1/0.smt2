; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/390.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.range "a" "b"))) (str.in_re s (re.++ (re.union (re.diff re.allchar _let_1) (re.++ _let_1 re.allchar)) (re.* re.allchar)))))
(assert (not (or (or (= s "a") (= s "b")) (= s ""))))
(check-sat)
(exit)