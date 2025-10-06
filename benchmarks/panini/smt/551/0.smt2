; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/551.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (re.diff re.allchar _let_1))) (let ((_let_3 (re.* _let_2))) (str.in_re s (re.union (re.++ (re.union (re.++ _let_2 (re.++ _let_3 _let_1)) (re.++ _let_1 re.allchar)) (re.* re.allchar)) _let_3))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (>= i@1 0))) (let ((_let_3 (and _let_2 (<= i@1 _let_1)))) (let ((_let_4 (< i@1 _let_1))) (let ((_let_5 (+ i@1 1))) (let ((_let_6 (and _let_2 _let_4))) (not (and (and (>= 0 0) (<= 0 _let_1)) (and (=> _let_4 (=> _let_3 (and _let_6 (and (=> _let_6 (or (distinct (str.at s i@1) "b") (> _let_1 1))) (and (>= _let_5 0) (<= _let_5 _let_1)))))) (=> (not _let_4) (=> _let_3 true))))))))))))
(check-sat)
(exit)