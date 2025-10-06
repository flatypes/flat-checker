; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/094.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.union (re.diff re.allchar _let_1) (re.++ _let_1 re.allchar)) (re.* re.allchar)))))
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.++ (re.union (re.diff re.allchar _let_1) (re.++ _let_1 re.allchar)) (re.* re.allchar)))) (let ((_let_3 (str.len s))) (let ((_let_4 (> _let_3 0))) (let ((_let_5 (str.substr s 0 (- 1 0)))) (not (and (=> _let_4 (and (and (>= 0 0) (>= 1 0)) (and (= _let_5 "a") (and (= _let_3 1) (str.in_re _let_5 _let_2))))) (=> (not _let_4) (str.in_re s _let_2))))))))))
(check-sat)
(exit)