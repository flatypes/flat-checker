; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/203.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.* _let_1) (re.diff re.allchar _let_1)))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (let ((_let_3 (>= _let_2 0))) (let ((_let_4 (and _let_3 (< _let_2 _let_1)))) (let ((_let_5 (str.substr s 0 (- _let_2 0)))) (let ((_let_6 (str.len _let_5))) (let ((_let_7 (>= i@1 0))) (let ((_let_8 (and _let_7 (<= i@1 _let_6)))) (let ((_let_9 (< i@1 _let_6))) (let ((_let_10 (+ i@1 1))) (let ((_let_11 (and _let_7 _let_9))) (let ((_let_12 (>= 0 0))) (not (and (and _let_12 _let_3) (and (and _let_12 (<= 0 _let_6)) (and (=> _let_9 (=> _let_8 (and _let_11 (and (=> _let_11 (= (str.at _let_5 i@1) "a")) (and (>= _let_10 0) (<= _let_10 _let_6)))))) (=> (not _let_9) (=> _let_8 (and _let_4 (=> _let_4 (distinct (str.at s _let_2) "a"))))))))))))))))))))))
(check-sat)
(exit)