; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/422.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.range "a" "c"))) (str.in_re s (re.union (re.++ _let_1 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (= _let_1 1))) (let ((_let_3 (str.at s 0))) (let ((_let_4 (distinct _let_3 "a"))) (let ((_let_5 (distinct _let_3 "b"))) (let ((_let_6 (distinct _let_3 "c"))) (not (and (and (>= 0 0) (< 0 _let_1)) (and (=> _let_4 (and (=> _let_5 (and (=> _let_6 (and false _let_2)) (=> (not _let_6) _let_2))) (=> (not _let_5) _let_2))) (=> (not _let_4) _let_2)))))))))))
(check-sat)
(exit)