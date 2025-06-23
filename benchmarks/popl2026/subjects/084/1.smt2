; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/084.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (not (= (str.substr s 0 (- 1 0)) "a")))
(check-sat)
(exit)