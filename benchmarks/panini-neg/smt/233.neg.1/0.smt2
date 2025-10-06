; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/233.neg.1.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.++ (re.* _let_2) (re.++ (re.diff re.allchar _let_2) (re.* re.allchar)))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (<= 1 i@1) (<= i@1 _let_1)))) (let ((_let_3 (< i@1 _let_1))) (let ((_let_4 (+ i@1 1))) (let ((_let_5 (str.substr s i@1 (- _let_1 i@1)))) (let ((_let_6 (>= _let_1 0))) (let ((_let_7 (and (>= i@1 0) _let_6))) (let ((_let_8 (+ 0 1))) (let ((_let_9 (str.substr s 0 (- _let_1 0)))) (let ((_let_10 (and (>= 0 0) _let_6))) (not (and _let_10 (and (=> _let_10 (str.contains _let_9 "a")) (and _let_10 (and (=> _let_10 (= (+ (str.indexof _let_9 "a" 0) 0) 0)) (and (and (<= 1 _let_8) (<= _let_8 _let_1)) (and (=> _let_3 (=> _let_2 (and _let_7 (and (=> _let_7 (str.contains _let_5 "b")) (and _let_7 (and (=> _let_7 (= (+ (str.indexof _let_5 "b" 0) i@1) i@1)) (and (<= 1 _let_4) (<= _let_4 _let_1)))))))) (=> (not _let_3) (=> _let_2 true))))))))))))))))))))
(check-sat)
(exit)