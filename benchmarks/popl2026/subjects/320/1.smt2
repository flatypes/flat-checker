; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/320.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b"))))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)