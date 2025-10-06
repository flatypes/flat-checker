; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/420.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.range "a" "c")))
(assert (not (or (or (= s "a") (= s "b")) (= s "c"))))
(check-sat)
(exit)