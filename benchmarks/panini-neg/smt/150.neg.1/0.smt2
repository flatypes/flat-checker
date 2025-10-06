; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/150.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.diff re.allchar _let_1))) (str.in_re s (re.union (re.++ _let_2 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ _let_1 (re.* _let_2))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 0 0) (< 0 _let_1)))) (not (and (= _let_1 1) (and _let_2 (=> _let_2 (distinct (str.at s 0) "a"))))))))
(check-sat)
(exit)