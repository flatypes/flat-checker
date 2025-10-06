; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/330.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (re.++ (str.to_re "a") (re.++ re.allchar (str.to_re "b"))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 2 0) (< 2 _let_1)))) (let ((_let_3 (and (>= 0 0) (< 0 _let_1)))) (not (=> (not (= _let_1 0)) (and _let_3 (and (=> _let_3 (= (str.at s 0) "a")) (and _let_2 (and (=> _let_2 (= (str.at s 2) "b")) (= _let_1 3)))))))))))
(check-sat)
(exit)