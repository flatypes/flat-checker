; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/272.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.union (str.to_re "") (str.to_re "b")))))
(assert (let ((_let_1 (= (str.indexof s "b" 0) 1))) (let ((_let_2 (str.len s))) (let ((_let_3 (and (>= 0 0) (< 0 _let_2)))) (not (and (=> _let_1 (and _let_3 (and (=> _let_3 (= (str.at s 0) "a")) (= _let_2 2)))) (=> (not _let_1) (= s "a"))))))))
(check-sat)
(exit)