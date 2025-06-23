; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/154.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "a"))))
(assert (distinct (str.at s 0) "a"))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)