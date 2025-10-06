; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/082.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_3 (= (str.at s 0) "a"))) (not (and (and _let_2 (=> (and _let_3 _let_2) (= _let_1 1))) (=> (and (not _let_3) _let_2) false)))))))
(check-sat)
(exit)