; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/150.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "a"))))
(assert (not (distinct (str.at s 0) "a")))
(check-sat)
(exit)