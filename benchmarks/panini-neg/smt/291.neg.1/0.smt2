; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/291.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (re.++ _let_1 re.allchar))) (str.in_re s (re.++ (re.union (re.diff re.allchar (re.range "a" "b")) (re.union (re.++ (str.to_re "a") (re.union (re.diff re.allchar _let_1) _let_2)) _let_2)) (re.* re.allchar))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (= _let_1 1))) (let ((_let_3 (str.at s 0))) (let ((_let_4 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_5 (= _let_3 "a"))) (let ((_let_6 (= _let_1 2))) (let ((_let_7 (and (>= 1 0) (< 1 _let_1)))) (not (=> (not (= _let_1 0)) (and (and _let_4 (=> (and _let_5 _let_4) (=> (not _let_2) (and (=> _let_6 (and _let_7 (=> _let_7 (= (str.at s 1) "b")))) (=> (not _let_6) false))))) (=> (and (not _let_5) _let_4) (and _let_4 (and (=> _let_4 (= _let_3 "b")) _let_2))))))))))))))
(check-sat)
(exit)