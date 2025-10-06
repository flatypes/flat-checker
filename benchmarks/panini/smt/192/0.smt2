; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/192.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.* (re.diff re.allchar _let_1)) _let_1))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (>= i@1 0))) (let ((_let_3 (and _let_2 (< i@1 _let_1)))) (let ((_let_4 (str.at s i@1))) (let ((_let_5 (- _let_1 1))) (let ((_let_6 (and _let_2 (<= i@1 _let_5)))) (let ((_let_7 (< i@1 _let_5))) (let ((_let_8 (+ i@1 1))) (let ((_let_9 (and (>= _let_8 0) (<= _let_8 _let_5)))) (let ((_let_10 (= _let_4 "a"))) (not (and (and (>= 0 0) (<= 0 _let_5)) (and (=> _let_7 (=> _let_6 (and (and _let_3 (=> (and _let_10 _let_3) (and false _let_9))) (=> (and (not _let_10) _let_3) _let_9)))) (=> (not _let_7) (=> _let_6 (and _let_3 (=> (and (distinct _let_4 "a") _let_3) false))))))))))))))))))
(check-sat)
(exit)