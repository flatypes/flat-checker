; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/273.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.++ (re.union (re.diff re.allchar _let_2) (re.++ _let_2 re.allchar)) (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (let ((_let_3 (str.at s _let_2))) (let ((_let_4 (= _let_3 "b"))) (let ((_let_5 (and (>= 0 0) (< 0 _let_1)))) (not (and (and (>= _let_2 0) (< _let_2 _let_1)) (and (=> _let_4 (and (= _let_1 2) (and _let_5 (=> _let_5 (= (str.at s 0) "a"))))) (=> (not _let_4) (and (= _let_3 "a") (= _let_1 1))))))))))))
(check-sat)
(exit)