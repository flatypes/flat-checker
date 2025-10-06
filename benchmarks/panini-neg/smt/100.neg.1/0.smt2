; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/100.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))) (str.in_re s (re.union (re.++ _let_1 (re.union (re.++ _let_1 (re.++ re.allchar (re.* re.allchar))) _let_2)) _let_2)))))
(assert (not (= s "aa")))
(check-sat)
(exit)