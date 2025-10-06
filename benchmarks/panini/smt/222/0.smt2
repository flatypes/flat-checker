; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/222.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (str.in_re s (re.++ (re.* (str.to_re "a")) (str.to_re "b"))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (let ((_let_3 (>= i@1 0))) (let ((_let_4 (and _let_3 (<= i@1 _let_2)))) (let ((_let_5 (< i@1 _let_2))) (let ((_let_6 (+ i@1 1))) (let ((_let_7 (and _let_3 (< i@1 _let_1)))) (not (and (str.contains s "b") (and (= (str.indexof s "b" 0) _let_2) (and (and (>= 0 0) (<= 0 _let_2)) (and (=> _let_5 (=> _let_4 (and _let_7 (and (=> _let_7 (= (str.at s i@1) "a")) (and (>= _let_6 0) (<= _let_6 _let_2)))))) (=> (not _let_5) (=> _let_4 true)))))))))))))))
(check-sat)
(exit)