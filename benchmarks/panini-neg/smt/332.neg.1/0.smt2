; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/332.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (re.* re.allchar))) (let ((_let_3 (str.to_re "a"))) (str.in_re s (re.union (re.++ (re.diff re.allchar _let_3) _let_2) (re.++ _let_3 (re.union (str.to_re "") (re.++ re.allchar (re.union (re.++ _let_1 (re.++ re.allchar _let_2)) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))))))))
(assert (let ((_let_1 (str.len s))) (not (=> (> _let_1 0) (and (= _let_1 3) (and (and (>= 0 0) (< 0 _let_1)) (and (and (>= 1 0) (< 1 _let_1)) (and (and (>= 2 0) (< 2 _let_1)) (and (= (str.at s 0) "a") (= (str.at s 2) "b"))))))))))
(check-sat)
(exit)