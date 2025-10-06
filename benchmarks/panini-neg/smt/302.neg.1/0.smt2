; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/302.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2)))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (let ((_let_1 (str.substr s 0 (- 2 0)))) (let ((_let_2 (str.len _let_1))) (let ((_let_3 (>= 0 0))) (not (and (and _let_3 (>= 2 0)) (and (= _let_2 2) (and (and _let_3 (< 0 _let_2)) (and (and (>= 1 0) (< 1 _let_2)) (and (= (str.at _let_1 0) "a") (= (str.at _let_1 1) "b")))))))))))
(check-sat)
(exit)