; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/262.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.union (str.to_re "") (str.to_re "a")) (str.to_re "b"))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (str.indexof s "b" 0))) (let ((_let_3 (= _let_2 1))) (let ((_let_4 (and (>= 0 0) (< 0 _let_1)))) (not (and (str.contains s "b") (and (=> _let_3 (and _let_4 (and (=> _let_4 (= (str.at s 0) "a")) (= _let_1 2)))) (=> (not _let_3) (and (= _let_2 0) (= _let_1 1)))))))))))
(check-sat)
(exit)