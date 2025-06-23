; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/154.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "a"))))
(assert (= (str.at s 0) "a"))
(assert (not false))
(check-sat)
(exit)