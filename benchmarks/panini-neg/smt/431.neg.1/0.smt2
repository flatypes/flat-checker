; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/431.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (re.diff re.allchar _let_1))) (str.in_re s (re.union (re.++ _let_2 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ _let_1 (re.* _let_2))))))))
(assert (let ((_let_1 (str.at s 0))) (let ((_let_2 (str.len s))) (not (and (= _let_2 1) (and (and (>= 0 0) (< 0 _let_2)) (=> (distinct _let_1 "a") (=> (distinct _let_1 "c") (distinct _let_1 "b")))))))))
(check-sat)
(exit)