; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/270.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") ((_ re.loop 0 1) (str.to_re "b")))))
(assert (not (or (= s "a") (= s "ab"))))
(check-sat)
(exit)