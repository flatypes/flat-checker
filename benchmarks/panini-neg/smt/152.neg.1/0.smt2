; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/152.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.diff re.allchar _let_1))) (str.in_re s (re.union (re.++ _let_2 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ _let_1 (re.* _let_2))))))))
(assert (not (and (= (str.len s) 1) (= (str.indexof s "a" 0) (- 1)))))
(check-sat)
(exit)