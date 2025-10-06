; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/108.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))) (str.in_re s (re.union (re.++ _let_1 (re.union (re.++ _let_1 (re.++ re.allchar (re.* re.allchar))) _let_2)) _let_2)))))
(assert (let ((_let_1 (str.len s))) (not (and (= _let_1 2) (and (and (>= 0 0) (< 0 _let_1)) (and (and (>= 1 0) (< 1 _let_1)) (and (= (str.at s 0) "a") (= (str.at s 1) "a"))))))))
(check-sat)
(exit)