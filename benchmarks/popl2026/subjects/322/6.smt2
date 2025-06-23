; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/322.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b"))))
(assert (not (= (str.at s 2) "b")))
(check-sat)
(exit)