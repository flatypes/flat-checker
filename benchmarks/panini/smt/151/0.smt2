; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/151.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "a"))))
(assert (not (and (= (str.len s) 1) (distinct s "a"))))
(check-sat)
(exit)