; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/086.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (not (and (>= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)