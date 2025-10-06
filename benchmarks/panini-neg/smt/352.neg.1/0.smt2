; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/352.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.* re.allchar))) (let ((_let_3 (str.to_re "b"))) (let ((_let_4 (re.diff re.allchar _let_3))) (str.in_re s (re.union (re.++ _let_1 (re.union (str.to_re "") (re.union (re.++ _let_4 (re.union (re.++ _let_3 (re.++ re.allchar _let_2)) (re.* (re.++ _let_4 (re.* _let_3))))) (re.++ _let_3 (re.++ (re.union _let_4 (re.++ _let_3 re.allchar)) _let_2))))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (= _let_1 3))) (let ((_let_3 (not _let_2))) (let ((_let_4 (= (str.at s 0) "a"))) (let ((_let_5 (and (>= 1 0) (< 1 _let_1)))) (let ((_let_6 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_7 (=> _let_2 (and _let_2 (and _let_6 (and _let_5 (and (and (>= 2 0) (< 2 _let_1)) (and _let_4 (= (str.at s 2) "b"))))))))) (let ((_let_8 (= _let_1 2))) (not (and (=> _let_8 (and _let_8 (and _let_6 (and _let_5 (and _let_7 (=> _let_3 (and _let_4 (= (str.at s 1) "b")))))))) (=> (not _let_8) (and _let_7 (=> _let_3 (and (= "" "a") (= "" "b"))))))))))))))))
(check-sat)
(exit)