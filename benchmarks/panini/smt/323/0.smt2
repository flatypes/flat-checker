; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/323.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.++ re.allchar (str.to_re "b")))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (str.substr s 1 (- _let_1 1)))) (let ((_let_3 (str.substr s 0 (- 2 0)))) (let ((_let_4 (str.len _let_2))) (let ((_let_5 (>= 1 0))) (let ((_let_6 (>= 0 0))) (let ((_let_7 (str.len _let_3))) (not (and (and _let_6 (>= 2 0)) (and (= _let_7 2) (and (and _let_6 (< 0 _let_7)) (and (and _let_5 (< 1 _let_7)) (and (and _let_5 (>= _let_1 0)) (and (= _let_4 2) (and (and _let_6 (< 0 _let_4)) (and (and _let_5 (< 1 _let_4)) (and (= (str.at _let_3 0) "a") (= (str.at _let_2 1) "b")))))))))))))))))))
(check-sat)
(exit)