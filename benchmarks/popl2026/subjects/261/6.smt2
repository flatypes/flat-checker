; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/261.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ ((_ re.loop 0 1) (str.to_re "a")) (str.to_re "b"))))
(assert (distinct (str.len s) 1))
(assert (= (str.len s) 2))
(assert (not (= (str.at s 1) "b")))
(check-sat)
(exit)