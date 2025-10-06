; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/103.py
(set-logic ALL)
(declare-const s String)
(declare-const j@1 Int)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ _let_1 _let_1))))
(assert (let ((_let_1 (= j@1 i@1))) (let ((_let_2 (str.len s))) (let ((_let_3 (and (<= 0 i@1) (<= i@1 _let_2)))) (let ((_let_4 (< i@1 _let_2))) (let ((_let_5 (+ i@1 1))) (let ((_let_6 (and (>= i@1 0) _let_4))) (not (and (and (and (<= 0 0) (<= 0 _let_2)) (= 0 0)) (and (=> _let_4 (=> _let_3 (=> _let_1 (and _let_6 (and (=> _let_6 (= (str.at s i@1) "a")) (and (and (<= 0 _let_5) (<= _let_5 _let_2)) (= (+ j@1 1) _let_5))))))) (=> (not _let_4) (=> _let_3 (=> _let_1 (= j@1 2))))))))))))))
(check-sat)
(exit)