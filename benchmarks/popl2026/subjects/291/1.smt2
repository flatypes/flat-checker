; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/291.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ ((_ re.loop 0 1) (str.to_re "a")) ((_ re.loop 0 1) (str.to_re "b")))))
(assert (distinct (str.len s) 0))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)