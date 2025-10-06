; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/232.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* (str.to_re "b")))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= i@1 1) (<= i@1 _let_1)))) (let ((_let_3 (< i@1 _let_1))) (let ((_let_4 (+ i@1 1))) (let ((_let_5 (and (>= i@1 0) _let_3))) (not (and (str.contains s "a") (and (= (str.indexof s "a" 0) 0) (and (and (>= 1 1) (<= 1 _let_1)) (and (=> _let_3 (=> _let_2 (and _let_5 (and (=> _let_5 (= (str.at s i@1) "b")) (and (>= _let_4 1) (<= _let_4 _let_1)))))) (=> (not _let_3) (=> _let_2 true)))))))))))))
(check-sat)
(exit)