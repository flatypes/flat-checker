; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/470.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "0"))) (str.in_re s (re.* (re.union _let_1 (re.++ _let_1 (str.to_re "1")))))))
(assert (let ((_let_1 (str.to_re "0"))) (let ((_let_2 (re.* (re.union _let_1 (re.++ _let_1 (str.to_re "1")))))) (let ((_let_3 (str.len s))) (let ((_let_4 (str.in_re (str.substr s i@1 (- _let_3 i@1)) _let_2))) (let ((_let_5 (and (<= 0 i@1) (<= i@1 _let_3)))) (let ((_let_6 (< i@1 _let_3))) (let ((_let_7 (+ i@1 1))) (let ((_let_8 (< _let_7 _let_3))) (let ((_let_9 (and (>= _let_7 0) _let_8))) (let ((_let_10 (= (str.at s _let_7) "1"))) (let ((_let_11 (+ i@1 2))) (let ((_let_12 (and (>= i@1 0) _let_6))) (not (and (and (and (<= 0 0) (<= 0 _let_3)) (str.in_re (str.substr s 0 (- _let_3 0)) _let_2)) (and (=> _let_6 (=> _let_5 (=> _let_4 (and _let_12 (and (=> _let_12 (= (str.at s i@1) "0")) (and (=> _let_8 (and _let_9 (=> (and _let_10 _let_9) (and (and (<= 0 _let_11) (<= _let_11 _let_3)) (str.in_re (str.substr s _let_11 (- _let_3 _let_11)) _let_2))))) (=> (and (not (and _let_8 _let_10)) _let_9) (and (and (<= 0 _let_7) (<= _let_7 _let_3)) (str.in_re (str.substr s _let_7 (- _let_3 _let_7)) _let_2))))))))) (=> (not _let_6) (=> _let_5 (=> _let_4 true)))))))))))))))))))
(check-sat)
(exit)