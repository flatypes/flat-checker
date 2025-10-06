; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/391.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (re.range "a" "b"))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (= _let_1 1))) (let ((_let_3 (str.at s 0))) (let ((_let_4 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_5 (= _let_3 "a"))) (not (=> (not (= _let_1 0)) (and (and _let_4 (=> (and _let_5 _let_4) _let_2)) (=> (and (not _let_5) _let_4) (and _let_4 (and (=> _let_4 (= _let_3 "b")) _let_2))))))))))))
(check-sat)
(exit)