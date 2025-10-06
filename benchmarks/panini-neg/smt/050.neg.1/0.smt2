; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/050.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "")))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)