; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/370.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "a") (str.to_re "b"))))
(assert (not (or (= s "a") (= s "b"))))
(check-sat)
(exit)