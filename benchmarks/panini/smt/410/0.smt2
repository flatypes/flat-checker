; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/410.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (str.in_re s (re.* (re.range "a" "b"))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (>= i@1 0))) (let ((_let_3 (and _let_2 (<= i@1 _let_1)))) (let ((_let_4 (< i@1 _let_1))) (let ((_let_5 (+ i@1 1))) (let ((_let_6 (str.at s i@1))) (let ((_let_7 (and _let_2 _let_4))) (let ((_let_8 (and _let_7 _let_7))) (not (and (and (>= 0 0) (<= 0 _let_1)) (and (=> _let_4 (=> _let_3 (and _let_8 (and (=> _let_8 (or (= _let_6 "a") (= _let_6 "b"))) (and (>= _let_5 0) (<= _let_5 _let_1)))))) (=> (not _let_4) (=> _let_3 true))))))))))))))
(check-sat)
(exit)