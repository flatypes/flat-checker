; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/391.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.range "a" "b"))) (str.in_re s (re.++ (re.union (re.diff re.allchar _let_1) (re.++ _let_1 re.allchar)) (re.* re.allchar)))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (= _let_1 1))) (let ((_let_3 (str.at s 0))) (let ((_let_4 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_5 (= _let_3 "a"))) (not (=> (not (= _let_1 0)) (and (and _let_4 (=> (and _let_5 _let_4) _let_2)) (=> (and (not _let_5) _let_4) (and _let_4 (and (=> _let_4 (= _let_3 "b")) _let_2))))))))))))
(check-sat)
(exit)