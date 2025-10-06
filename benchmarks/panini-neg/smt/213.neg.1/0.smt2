; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/213.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.union (re.++ _let_2 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2))))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (let ((_let_1 (=> (distinct (str.at s 1) "b") false))) (let ((_let_2 (distinct (str.at s 0) "a"))) (let ((_let_3 (str.len s))) (not (and (= _let_3 2) (and (and (>= 0 0) (< 0 _let_3)) (and (and (>= 1 0) (< 1 _let_3)) (and (=> _let_2 (and false _let_1)) (=> (not _let_2) _let_1))))))))))
(check-sat)
(exit)