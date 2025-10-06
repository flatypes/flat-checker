; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/172.neg.1.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.* (re.diff re.allchar _let_1)) (re.++ _let_1 (re.* re.allchar))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (>= i@1 0))) (let ((_let_3 (and _let_2 (<= i@1 _let_1)))) (let ((_let_4 (< i@1 _let_1))) (let ((_let_5 (+ i@1 1))) (let ((_let_6 (and (>= _let_5 0) (<= _let_5 _let_1)))) (let ((_let_7 (and _let_2 _let_4))) (let ((_let_8 (= (str.at s i@1) "a"))) (not (and (and (>= 0 0) (<= 0 _let_1)) (and (=> _let_4 (=> _let_3 (and (and _let_7 (=> (and _let_8 _let_7) (and false _let_6))) (=> (and (not _let_8) _let_7) _let_6)))) (=> (not _let_4) (=> _let_3 true))))))))))))))
(check-sat)
(exit)