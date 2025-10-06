; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/530.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.* (re.union (re.diff re.allchar _let_1) (re.++ _let_1 (str.to_re "b")))))))
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.* (re.union (re.diff re.allchar _let_1) (re.++ _let_1 (str.to_re "b")))))) (let ((_let_3 (str.len s))) (let ((_let_4 (str.in_re (str.substr s i@1 (- _let_3 i@1)) _let_2))) (let ((_let_5 (and (<= 0 i@1) (<= i@1 _let_3)))) (let ((_let_6 (< i@1 _let_3))) (let ((_let_7 (+ i@1 1))) (let ((_let_8 (and (>= i@1 0) _let_6))) (let ((_let_9 (= (str.at s i@1) "a"))) (let ((_let_10 (+ i@1 2))) (let ((_let_11 (and (>= _let_7 0) (< _let_7 _let_3)))) (not (and (and (and (<= 0 0) (<= 0 _let_3)) (str.in_re (str.substr s 0 (- _let_3 0)) _let_2)) (and (=> _let_6 (=> _let_5 (=> _let_4 (and (and _let_8 (=> (and _let_9 _let_8) (and _let_11 (and (=> _let_11 (= (str.at s _let_7) "b")) (and (and (<= 0 _let_10) (<= _let_10 _let_3)) (str.in_re (str.substr s _let_10 (- _let_3 _let_10)) _let_2)))))) (=> (and (not _let_9) _let_8) (and (and (<= 0 _let_7) (<= _let_7 _let_3)) (str.in_re (str.substr s _let_7 (- _let_3 _let_7)) _let_2))))))) (=> (not _let_6) (=> _let_5 (=> _let_4 true))))))))))))))))))
(check-sat)
(exit)