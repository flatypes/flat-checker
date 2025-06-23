; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/323.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b"))))
(assert (not (and (>= 1 0) (< 1 (str.len (str.substr s 1 (- (str.len s) 1)))))))
(check-sat)
(exit)