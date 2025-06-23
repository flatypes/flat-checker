; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/083.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (distinct (str.at s 0) "a"))
(assert (not false))
(check-sat)
(exit)