; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/290.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (re.++ _let_1 re.allchar))) (str.in_re s (re.++ (re.union (re.diff re.allchar (re.range "a" "b")) (re.union (re.++ (str.to_re "a") (re.union (re.diff re.allchar _let_1) _let_2)) _let_2)) (re.* re.allchar))))))
(assert (not (or (or (or (= s "") (= s "a")) (= s "b")) (= s "ab"))))
(check-sat)
(exit)