; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/526.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ _let_1 (re.++ _let_1 (re.* re.allchar))))))
(assert (let ((_let_1 (str.substr s 0 (- 2 0)))) (let ((_let_2 (str.at _let_1 0))) (let ((_let_3 (str.len _let_1))) (let ((_let_4 (>= 0 0))) (not (and (and _let_4 (>= 2 0)) (and (= _let_3 2) (and (and _let_4 (< 0 _let_3)) (and (and (>= 1 0) (< 1 _let_3)) (and (= _let_2 (str.at _let_1 1)) (= _let_2 "a"))))))))))))
(check-sat)
(exit)