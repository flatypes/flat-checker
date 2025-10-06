; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/073.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.++ re.allchar (re.* re.allchar)))))
(assert (let ((_let_1 (str.len s))) (not (and (and (>= 0 0) (< 0 _let_1)) (and (>= 1 0) (< 1 _let_1))))))
(check-sat)
(exit)