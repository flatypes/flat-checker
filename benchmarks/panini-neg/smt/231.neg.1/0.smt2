; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/231.neg.1.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.++ (re.* _let_2) (re.++ (re.diff re.allchar _let_2) (re.* re.allchar)))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (let ((_let_1 (str.at s i@1))) (let ((_let_2 (str.len s))) (let ((_let_3 (>= i@1 0))) (let ((_let_4 (and _let_3 (< i@1 _let_2)))) (let ((_let_5 (and (<= i@1 _let_2) _let_3))) (let ((_let_6 (> i@1 0))) (let ((_let_7 (- i@1 1))) (let ((_let_8 (>= _let_7 0))) (let ((_let_9 (and (<= _let_7 _let_2) _let_8))) (let ((_let_10 (and _let_8 (< _let_7 _let_2)))) (let ((_let_11 (distinct (str.at s _let_7) "b"))) (not (and (and (<= _let_2 _let_2) (>= _let_2 0)) (and (=> _let_6 (=> _let_5 (and (and _let_10 (=> (and _let_11 _let_10) _let_9)) (=> (and (not _let_11) _let_10) _let_9)))) (=> (or (not _let_6) (and (distinct _let_1 "b") _let_4)) (=> _let_5 (and _let_4 (=> _let_4 (= _let_1 "a"))))))))))))))))))))
(check-sat)
(exit)