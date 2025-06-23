; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/330.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (distinct (str.len s) 0))
(assert (not (and (>= 2 0) (< 2 (str.len s)))))
(check-sat)
(exit)