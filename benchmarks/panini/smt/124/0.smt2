; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/124.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* re.allchar))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (>= i@1 0))) (let ((_let_3 (and _let_2 (<= i@1 _let_1)))) (let ((_let_4 (< i@1 _let_1))) (let ((_let_5 (+ i@1 1))) (let ((_let_6 (>= 0 0))) (let ((_let_7 (and _let_6 (< 0 _let_1)))) (not (and _let_7 (and (=> _let_7 (= (str.at s 0) "a")) (and (and _let_6 (<= 0 _let_1)) (and (=> _let_4 (=> _let_3 (and (and _let_2 _let_4) (and (>= _let_5 0) (<= _let_5 _let_1))))) (=> (not _let_4) (=> _let_3 true)))))))))))))))
(check-sat)
(exit)