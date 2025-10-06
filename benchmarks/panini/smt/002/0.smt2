; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/002.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "")))
(assert (not (=> (distinct s "") false)))
(check-sat)
(exit)