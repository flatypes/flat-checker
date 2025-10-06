; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/411.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (str.in_re s (re.* (re.union (str.to_re "a") (str.to_re "b")))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (<= 0 i@1) (<= i@1 _let_1)))) (let ((_let_3 (< i@1 _let_1))) (let ((_let_4 (+ i@1 0))) (let ((_let_5 (and (> 0 0) (and (<= 0 _let_4) (<= _let_4 _let_1))))) (let ((_let_6 (< 0 (- _let_1 i@1)))) (let ((_let_7 (and (>= _let_4 0) (< _let_4 _let_1)))) (let ((_let_8 (= (str.at s _let_4) "b"))) (let ((_let_9 (+ 0 1))) (let ((_let_10 (+ i@1 _let_9))) (let ((_let_11 (and (>= i@1 0) _let_3))) (let ((_let_12 (= (str.at s i@1) "a"))) (let ((_let_13 (+ i@1 1))) (not (and (and (<= 0 0) (<= 0 _let_1)) (and (=> _let_3 (=> _let_2 (and (and _let_11 (=> (and _let_12 _let_11) (and (<= 0 _let_13) (<= _let_13 _let_1)))) (=> (and (not _let_12) _let_11) (and (=> _let_6 (and (and _let_7 (=> (and _let_8 _let_7) (and (> _let_9 0) (and (<= 0 _let_10) (<= _let_10 _let_1))))) (=> (and (not _let_8) _let_7) _let_5))) (=> (not _let_6) _let_5)))))) (=> (not _let_3) (=> _let_2 true)))))))))))))))))))
(check-sat)
(exit)