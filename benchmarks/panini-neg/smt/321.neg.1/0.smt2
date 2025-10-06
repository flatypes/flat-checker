; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/321.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.union (str.to_re "") (re.++ re.allchar (re.union (re.++ _let_2 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2))))))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 2 0) (< 2 _let_1)))) (let ((_let_3 (and (>= 0 0) (< 0 _let_1)))) (not (=> (not (= s "acb")) (and (= _let_1 3) (and _let_3 (and (=> _let_3 (= (str.at s 0) "a")) (and _let_2 (=> _let_2 (= (str.at s 2) "b"))))))))))))
(check-sat)
(exit)