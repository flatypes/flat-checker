; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/081.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.union (re.++ _let_1 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 0 0) (< 0 _let_1)))) (not (and _let_2 (and (=> _let_2 (= (str.at s 0) "a")) (= _let_1 1)))))))
(check-sat)
(exit)