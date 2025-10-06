; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/113.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (str.in_re s (re.* (str.to_re "a"))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (>= i@1 0))) (let ((_let_3 (and _let_2 (<= i@1 _let_1)))) (let ((_let_4 (< i@1 _let_1))) (let ((_let_5 (and _let_2 _let_4))) (let ((_let_6 (distinct (str.at s i@1) "a"))) (let ((_let_7 (and _let_6 _let_5))) (let ((_let_8 (+ i@1 1))) (not (and (and (>= 0 0) (<= 0 _let_1)) (and (=> _let_4 (=> _let_3 (and (and _let_5 (=> _let_7 _let_3)) (=> (and (not _let_6) _let_5) (and (>= _let_8 0) (<= _let_8 _let_1)))))) (=> (or (not _let_4) _let_7) (=> _let_3 (= i@1 _let_1)))))))))))))))
(check-sat)
(exit)