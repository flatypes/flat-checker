; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/352.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") ((_ re.loop 0 1) re.allchar)) (str.to_re "b"))))
(assert (= (str.len s) 2))
(assert (not (= (str.len s) 2)))
(check-sat)
(exit)