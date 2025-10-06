; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/211.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.union (re.++ _let_2 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2))))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 1 0) (< 1 _let_1)))) (let ((_let_3 (and (>= 0 0) (< 0 _let_1)))) (not (and _let_3 (and (=> _let_3 (= (str.at s 0) "a")) (and _let_2 (and (=> _let_2 (= (str.at s 1) "b")) (= _let_1 2))))))))))
(check-sat)
(exit)