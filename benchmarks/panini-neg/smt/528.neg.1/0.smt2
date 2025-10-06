; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/528.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.union (str.to_re "") _let_1) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (let ((_let_1 (>= 1 0))) (let ((_let_2 (and _let_1 (< 1 (str.len s))))) (let ((_let_3 (str.substr s 0 (- 2 0)))) (let ((_let_4 (str.len _let_3))) (let ((_let_5 (>= 0 0))) (not (and (and _let_5 (>= 2 0)) (and (= _let_4 2) (and (and _let_5 (< 0 _let_4)) (and (and _let_1 (< 1 _let_4)) (and (= (str.at _let_3 0) (str.at _let_3 1)) (and _let_2 (=> _let_2 (= (str.at s 1) "a")))))))))))))))
(check-sat)
(exit)