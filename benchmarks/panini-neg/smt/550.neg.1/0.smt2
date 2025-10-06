; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/550.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "b")))
(assert (not (or (or (= s "a") (distinct s "b")) (= s "c"))))
(check-sat)
(exit)