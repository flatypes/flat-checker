; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/126.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* re.allchar))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (>= i@1 0))) (let ((_let_3 (and _let_2 (< i@1 _let_1)))) (let ((_let_4 (and (<= i@1 _let_1) _let_2))) (let ((_let_5 (> i@1 0))) (let ((_let_6 (- i@1 1))) (not (and (and (<= _let_1 _let_1) (>= _let_1 0)) (and (=> _let_5 (=> _let_4 (and (<= _let_6 _let_1) (>= _let_6 0)))) (=> (not _let_5) (=> _let_4 (and _let_3 (=> _let_3 (= (str.at s i@1) "a")))))))))))))))
(check-sat)
(exit)