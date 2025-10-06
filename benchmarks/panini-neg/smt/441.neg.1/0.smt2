; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/441.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "c"))) (let ((_let_2 (str.to_re "a"))) (let ((_let_3 (re.union _let_2 _let_1))) (let ((_let_4 (re.++ re.allchar (re.* re.allchar)))) (let ((_let_5 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_2 (re.union (re.++ _let_5 _let_4) (re.* (re.++ (re.diff re.allchar _let_5) (re.* _let_5))))) (re.union (re.++ _let_1 _let_4) (re.* (re.++ (re.diff re.allchar _let_3) (re.* _let_3))))))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (str.at s 0))) (let ((_let_3 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_4 (= _let_2 "a"))) (let ((_let_5 (and (>= 1 0) (< 1 _let_1)))) (not (and (and _let_3 (=> (and _let_4 _let_3) (and _let_5 (and (=> _let_5 (= (str.at s 1) "b")) (<= _let_1 2))))) (=> (and (not _let_4) _let_3) (and _let_3 (and (=> _let_3 (= _let_2 "c")) (= _let_1 1))))))))))))
(check-sat)
(exit)