; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/063.py
(set-logic ALL)
(declare-const i@1 Int)
(assert (< i@1 3))
(assert (>= i@1 0))
(assert (<= i@1 3))
(assert (let ((_let_1 (+ i@1 1))) (not (and (>= _let_1 0) (<= _let_1 3)))))
(check-sat)
(exit)