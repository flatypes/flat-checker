; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/085.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (let ((_let_1 (str.at s 0))) (let ((_let_2 (str.len s))) (let ((_let_3 (and (>= 0 0) (< 0 _let_2)))) (let ((_let_4 (and _let_3 (and (=> _let_3 (= _let_1 "a")) (and _let_3 (=> _let_3 (str.in_re _let_1 (str.to_re "a")))))))) (let ((_let_5 (not (= _let_2 1)))) (not (and (=> _let_5 (and false _let_4)) (=> (not _let_5) _let_4)))))))))
(check-sat)
(exit)