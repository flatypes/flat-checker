; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/085.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (distinct (str.len s) 1))
(assert (not (= (str.at s 0) "a")))
(check-sat)
(exit)