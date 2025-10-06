; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/123.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* re.allchar))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (>= i@1 0))) (let ((_let_3 (and _let_2 (<= i@1 _let_1)))) (let ((_let_4 (< i@1 _let_1))) (let ((_let_5 (+ i@1 1))) (let ((_let_6 (and _let_2 _let_4))) (not (and (> _let_1 0) (and (and (>= 0 0) (<= 0 _let_1)) (and (=> _let_4 (=> _let_3 (and _let_6 (and (=> _let_6 (or (> i@1 0) (= (str.at s i@1) "a"))) (and (>= _let_5 0) (<= _let_5 _let_1)))))) (=> (not _let_4) (=> _let_3 true)))))))))))))
(check-sat)
(exit)