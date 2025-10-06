; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/060.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.++ re.allchar re.allchar))))
(assert (not (= (str.len s) 3)))
(check-sat)
(exit)