; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/490.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "1"))) (str.in_re s (re.++ (re.* (re.range "0" "1")) (re.++ (str.to_re "0") (re.++ _let_1 _let_1))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 3))) (let ((_let_3 (and (>= _let_2 0) (>= _let_1 0)))) (let ((_let_4 (>= i@1 0))) (let ((_let_5 (and _let_4 (<= i@1 _let_1)))) (let ((_let_6 (< i@1 _let_1))) (let ((_let_7 (+ i@1 1))) (let ((_let_8 (str.at s i@1))) (let ((_let_9 (and _let_4 _let_6))) (let ((_let_10 (and _let_9 _let_9))) (not (and (and (>= 0 0) (<= 0 _let_1)) (and (=> _let_6 (=> _let_5 (and _let_10 (and (=> _let_10 (or (= _let_8 "0") (= _let_8 "1"))) (and (>= _let_7 0) (<= _let_7 _let_1)))))) (=> (not _let_6) (=> _let_5 (and _let_3 (=> _let_3 (= (str.substr s _let_2 (- _let_1 _let_2)) "011")))))))))))))))))))
(check-sat)
(exit)