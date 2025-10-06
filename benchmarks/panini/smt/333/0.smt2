; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/333.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (re.++ (str.to_re "a") (re.++ re.allchar (str.to_re "b"))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (=> (> _let_1 3) false))) (let ((_let_3 (> _let_1 1))) (let ((_let_4 (str.substr s 1 (- 3 1)))) (let ((_let_5 (str.len _let_4))) (let ((_let_6 (>= 1 0))) (let ((_let_7 (>= 0 0))) (let ((_let_8 (and (=> _let_3 (and (and _let_6 (>= 3 0)) (and (= _let_5 2) (and (and _let_7 (< 0 _let_5)) (and (and _let_6 (< 1 _let_5)) (and (= (str.at _let_4 1) "b") _let_2)))))) (=> (not _let_3) _let_2)))) (let ((_let_9 (> _let_1 0))) (let ((_let_10 (str.substr s 0 (- 2 0)))) (let ((_let_11 (str.len _let_10))) (not (and (=> _let_9 (and (and _let_7 (>= 2 0)) (and (= _let_11 2) (and (and _let_7 (< 0 _let_11)) (and (and _let_6 (< 1 _let_11)) (and (= (str.at _let_10 0) "a") _let_8)))))) (=> (not _let_9) _let_8)))))))))))))))
(check-sat)
(exit)