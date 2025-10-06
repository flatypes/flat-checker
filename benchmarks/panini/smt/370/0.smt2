; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/370.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.range "a" "b")))
(assert (not (or (= s "a") (= s "b"))))
(check-sat)
(exit)