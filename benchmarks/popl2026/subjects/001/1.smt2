; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/001.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "")))
(assert (not (= s "")))
(check-sat)
(exit)