; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/263.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.union (str.to_re "") (str.to_re "a")) (str.to_re "b"))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (= _let_1 2))) (let ((_let_3 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_4 (- _let_1 1))) (not (and (and (>= _let_4 0) (< _let_4 _let_1)) (and (= (str.at s _let_4) "b") (and (=> _let_2 (and _let_3 (=> _let_3 (= (str.at s 0) "a")))) (=> (not _let_2) (= _let_1 1)))))))))))
(check-sat)
(exit)