; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/073.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") re.allchar)))
(assert (let ((_let_1 (str.len s))) (not (and (and (>= 0 0) (< 0 _let_1)) (and (>= 1 0) (< 1 _let_1))))))
(check-sat)
(exit)