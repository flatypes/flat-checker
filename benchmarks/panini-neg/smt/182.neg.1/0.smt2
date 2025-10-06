; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/182.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.diff re.allchar _let_1))) (str.in_re s (re.union (re.++ _let_2 (re.union (re.++ _let_1 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ _let_2 (re.* _let_1))))) (re.* (re.++ _let_1 (re.* _let_2))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 1 0) (< 1 _let_1)))) (let ((_let_3 (= (str.at s 1) "a"))) (let ((_let_4 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_5 (= (str.at s 0) "a"))) (not (and (and _let_4 (=> (and _let_5 _let_4) false)) (=> (and (not _let_5) _let_4) (and (and _let_2 (=> (and _let_3 _let_2) (= _let_1 2))) (=> (and (not _let_3) _let_2) false)))))))))))
(check-sat)
(exit)