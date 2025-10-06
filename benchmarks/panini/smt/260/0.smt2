; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/260.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.union (str.to_re "") (str.to_re "a")) (str.to_re "b"))))
(assert (not (or (= s "b") (= s "ab"))))
(check-sat)
(exit)