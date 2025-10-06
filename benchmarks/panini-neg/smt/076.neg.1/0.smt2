; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/076.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") re.allchar)))
(assert (not (and (>= 0 0) (>= (- (str.len s) 2) 0))))
(check-sat)
(exit)