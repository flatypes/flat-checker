; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/031.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.* re.allchar)))
(assert (not (>= (str.len s) 0)))
(check-sat)
(exit)