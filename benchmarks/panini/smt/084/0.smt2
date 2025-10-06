; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/084.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (not (and (and (>= 0 0) (>= 1 0)) (and (= (str.substr s 0 (- 1 0)) "a") (= (str.len s) 1)))))
(check-sat)
(exit)