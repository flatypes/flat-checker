; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/351.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.++ (re.union (str.to_re "") re.allchar) (str.to_re "b")))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 2 0) (< 2 _let_1)))) (let ((_let_3 (and (>= 1 0) (< 1 _let_1)))) (let ((_let_4 (and (>= 0 0) (< 0 _let_1)))) (not (and _let_4 (and (=> _let_4 (= (str.at s 0) "a")) (and _let_3 (=> (and (not (and (= (str.at s 1) "b") (= _let_1 2))) _let_3) (and _let_2 (and (=> _let_2 (= (str.at s 2) "b")) (= _let_1 3)))))))))))))
(check-sat)
(exit)