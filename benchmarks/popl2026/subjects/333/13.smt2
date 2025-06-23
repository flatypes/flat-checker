; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/333.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (<= (str.len s) 0))
(assert (> (str.len s) 1))
(assert (not (and (>= 1 0) (< 1 (str.len (str.substr s 1 (- 3 1)))))))
(check-sat)
(exit)