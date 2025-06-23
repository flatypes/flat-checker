; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/522.py
(set-logic ALL)
(declare-const i@1 Int)
(assert (< i@1 2))
(assert (>= i@1 0))
(assert (<= i@1 2))
(assert (let ((_let_1 (+ i@1 1))) (not (and (>= _let_1 0) (<= _let_1 2)))))
(check-sat)
(exit)