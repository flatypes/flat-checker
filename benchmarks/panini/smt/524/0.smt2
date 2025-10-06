; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/524.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ _let_1 (re.++ _let_1 (re.* re.allchar))))))
(assert (let ((_let_1 (str.substr s 0 (- 2 0)))) (let ((_let_2 (str.len _let_1))) (let ((_let_3 (>= 0 0))) (not (and (and _let_3 (>= 2 0)) (and (= _let_2 2) (and (and _let_3 (< 0 _let_2)) (and (and (>= 1 0) (< 1 _let_2)) (and (= (str.at _let_1 0) "a") (= (str.at _let_1 1) "a")))))))))))
(check-sat)
(exit)