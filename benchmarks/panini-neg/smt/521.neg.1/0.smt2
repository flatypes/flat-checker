; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/521.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.union (str.to_re "") _let_1) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (let ((_let_1 (str.at s 0))) (let ((_let_2 (str.len s))) (let ((_let_3 (and (>= 0 0) (< 0 _let_2)))) (let ((_let_4 (and (and (>= 1 0) (< 1 _let_2)) _let_3))) (not (and _let_3 (and (=> _let_3 (= _let_1 "a")) (and _let_4 (=> _let_4 (= (str.at s 1) _let_1)))))))))))
(check-sat)
(exit)