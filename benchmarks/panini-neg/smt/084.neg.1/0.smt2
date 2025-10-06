; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/084.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.union (re.++ _let_1 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (not (and (and (>= 0 0) (>= 1 0)) (and (= (str.substr s 0 (- 1 0)) "a") (= (str.len s) 1)))))
(check-sat)
(exit)