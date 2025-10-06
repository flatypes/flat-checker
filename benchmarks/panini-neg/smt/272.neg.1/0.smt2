; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/272.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.++ (re.union (re.diff re.allchar _let_2) (re.++ _let_2 re.allchar)) (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (let ((_let_1 (= (str.indexof s "b" 0) 1))) (let ((_let_2 (str.len s))) (let ((_let_3 (and (>= 0 0) (< 0 _let_2)))) (not (and (=> _let_1 (and _let_3 (and (=> _let_3 (= (str.at s 0) "a")) (= _let_2 2)))) (=> (not _let_1) (= s "a"))))))))
(check-sat)
(exit)