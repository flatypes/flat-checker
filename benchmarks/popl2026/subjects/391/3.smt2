; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/391.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.union (str.to_re "a") (str.to_re "b")))))
(assert (distinct (str.len s) 0))
(assert (distinct (str.at s 0) "a"))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)