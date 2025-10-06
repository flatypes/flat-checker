; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/261.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.range "a" "b"))) (let ((_let_2 (str.to_re "b"))) (let ((_let_3 (re.++ _let_2 (re.++ re.allchar (re.* re.allchar))))) (str.in_re s (re.union (re.++ (str.to_re "a") (re.union _let_3 (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2))))) (re.union _let_3 (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (= _let_1 2))) (let ((_let_3 (and (>= 1 0) (< 1 _let_1)))) (let ((_let_4 (str.at s 0))) (let ((_let_5 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_6 (= _let_1 1))) (not (and (=> _let_6 (and _let_5 (=> _let_5 (= _let_4 "b")))) (=> (not _let_6) (and (=> _let_2 (and _let_5 (and (=> _let_5 (= _let_4 "a")) (and _let_3 (=> _let_3 (= (str.at s 1) "b")))))) (=> (not _let_2) false))))))))))))
(check-sat)
(exit)