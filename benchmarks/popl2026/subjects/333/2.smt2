; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/333.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (> (str.len s) 0))
(assert (not (and (>= 0 0) (< 0 (str.len (str.substr s 0 (- 2 0)))))))
(check-sat)
(exit)