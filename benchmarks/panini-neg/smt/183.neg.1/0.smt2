; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/183.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.diff re.allchar _let_1))) (str.in_re s (re.union (re.++ _let_2 (re.union (re.++ _let_1 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ _let_2 (re.* _let_1))))) (re.* (re.++ _let_1 (re.* _let_2))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 2))) (let ((_let_3 (and (>= _let_2 0) (< _let_2 _let_1)))) (let ((_let_4 (- _let_1 1))) (let ((_let_5 (and (>= _let_4 0) (< _let_4 _let_1)))) (not (and _let_5 (and (=> _let_5 (= (str.at s _let_4) "a")) (and _let_3 (and (=> _let_3 (distinct (str.at s _let_2) "a")) (= _let_1 2))))))))))))
(check-sat)
(exit)