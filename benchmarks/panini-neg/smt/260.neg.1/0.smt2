; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/260.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.range "a" "b"))) (let ((_let_2 (str.to_re "b"))) (let ((_let_3 (re.++ _let_2 (re.++ re.allchar (re.* re.allchar))))) (str.in_re s (re.union (re.++ (str.to_re "a") (re.union _let_3 (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2))))) (re.union _let_3 (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))))
(assert (not (or (= s "b") (= s "ab"))))
(check-sat)
(exit)