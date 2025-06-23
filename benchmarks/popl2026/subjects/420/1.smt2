; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/420.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "a") (str.to_re "b") (str.to_re "c"))))
(assert (not (or (or (= s "a") (= s "b")) (= s "c"))))
(check-sat)
(exit)