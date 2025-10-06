; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/081.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 0 0) (< 0 _let_1)))) (not (and _let_2 (and (=> _let_2 (= (str.at s 0) "a")) (= _let_1 1)))))))
(check-sat)
(exit)