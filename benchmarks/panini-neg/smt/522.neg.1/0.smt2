; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/522.neg.1.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.union (str.to_re "") _let_1) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (let ((_let_1 (>= i@1 0))) (let ((_let_2 (and _let_1 (<= i@1 2)))) (let ((_let_3 (< i@1 2))) (let ((_let_4 (+ i@1 1))) (let ((_let_5 (and _let_1 (< i@1 (str.len s))))) (not (and (and (>= 0 0) (<= 0 2)) (and (=> _let_3 (=> _let_2 (and _let_5 (and (=> _let_5 (= (str.at s i@1) "a")) (and (>= _let_4 0) (<= _let_4 2)))))) (=> (not _let_3) (=> _let_2 true)))))))))))
(check-sat)
(exit)