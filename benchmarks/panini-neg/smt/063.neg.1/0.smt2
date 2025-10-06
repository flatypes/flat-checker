; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/063.neg.1.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re ""))) (str.in_re s (re.union _let_1 (re.++ re.allchar (re.union _let_1 (re.++ re.allchar (re.union _let_1 (re.++ re.allchar (re.++ re.allchar (re.* re.allchar)))))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (>= i@1 0))) (let ((_let_3 (and _let_2 (<= i@1 3)))) (let ((_let_4 (< i@1 3))) (let ((_let_5 (+ i@1 1))) (not (and (and (>= 0 0) (<= 0 3)) (and (=> _let_4 (=> _let_3 (and (and _let_2 (< i@1 _let_1)) (and (>= _let_5 0) (<= _let_5 3))))) (=> (not _let_4) (=> _let_3 (= _let_1 i@1))))))))))))
(check-sat)
(exit)