; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/001.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "")))
(assert (not (= s "")))
(check-sat)
(exit)