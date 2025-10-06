; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/040.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re ""))) (str.in_re s (re.union _let_1 (re.++ re.allchar (re.union _let_1 (re.++ re.allchar (re.++ re.allchar (re.* re.allchar)))))))))
(assert (not (= (str.len s) 2)))
(check-sat)
(exit)