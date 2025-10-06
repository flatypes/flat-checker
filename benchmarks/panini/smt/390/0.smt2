; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/390.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (re.range "a" "b"))))
(assert (not (or (or (= s "a") (= s "b")) (= s ""))))
(check-sat)
(exit)