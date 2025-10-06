; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/070.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.++ re.allchar (re.* re.allchar)))))
(assert (not (>= (str.len s) 2)))
(check-sat)
(exit)