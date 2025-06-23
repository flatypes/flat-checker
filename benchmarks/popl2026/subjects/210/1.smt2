; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/210.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (str.to_re "b"))))
(assert (not (= s "ab")))
(check-sat)
(exit)