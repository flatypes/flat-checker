; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/273.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.union (str.to_re "") (str.to_re "b")))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (let ((_let_3 (str.at s _let_2))) (let ((_let_4 (= _let_3 "b"))) (let ((_let_5 (and (>= 0 0) (< 0 _let_1)))) (not (and (and (>= _let_2 0) (< _let_2 _let_1)) (and (=> _let_4 (and (= _let_1 2) (and _let_5 (=> _let_5 (= (str.at s 0) "a"))))) (=> (not _let_4) (and (= _let_3 "a") (= _let_1 1))))))))))))
(check-sat)
(exit)