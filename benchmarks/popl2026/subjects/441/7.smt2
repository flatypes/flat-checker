; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/441.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "c") (re.++ (str.to_re "a") (str.to_re "b")))))
(assert (distinct (str.at s 0) "a"))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)