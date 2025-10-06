; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/221.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (str.in_re s (re.++ (re.* (str.to_re "a")) (str.to_re "b"))))
(assert (let ((_let_1 (str.at s i@1))) (let ((_let_2 (str.len s))) (let ((_let_3 (< i@1 _let_2))) (let ((_let_4 (and (>= i@1 0) _let_3))) (let ((_let_5 (and (<= 0 i@1) _let_3))) (let ((_let_6 (distinct _let_1 "a"))) (let ((_let_7 (and _let_6 _let_4))) (let ((_let_8 (+ i@1 1))) (not (and (and (<= 0 0) (< 0 _let_2)) (and (=> _let_3 (=> _let_5 (and (and _let_4 (=> _let_7 _let_5)) (=> (and (not _let_6) _let_4) (and (<= 0 _let_8) (< _let_8 _let_2)))))) (=> (or (not _let_3) _let_7) (=> _let_5 (and _let_4 (=> _let_4 (= _let_1 "b")))))))))))))))))
(check-sat)
(exit)