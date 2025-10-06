; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/085.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.union (re.++ _let_1 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.at s 0))) (let ((_let_3 (str.len s))) (let ((_let_4 (and (>= 0 0) (< 0 _let_3)))) (let ((_let_5 (and _let_4 (and (=> _let_4 (= _let_2 "a")) (and _let_4 (=> _let_4 (str.in_re _let_2 (re.union (re.++ _let_1 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))))) (let ((_let_6 (not (= _let_3 1)))) (not (and (=> _let_6 (and false _let_5)) (=> (not _let_6) _let_5))))))))))
(check-sat)
(exit)