; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/092.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (str.to_re "a"))))
(assert (not (or (= (str.len s) 0) (= s "a"))))
(check-sat)
(exit)