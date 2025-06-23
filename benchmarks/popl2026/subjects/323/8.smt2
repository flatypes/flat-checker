; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/323.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b"))))
(assert (not (= (str.at (str.substr s 1 (- (str.len s) 1)) 1) "b")))
(check-sat)
(exit)