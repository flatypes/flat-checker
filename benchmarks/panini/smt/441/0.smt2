; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/441.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "c") (re.++ (str.to_re "a") (str.to_re "b")))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (str.at s 0))) (let ((_let_3 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_4 (= _let_2 "a"))) (let ((_let_5 (and (>= 1 0) (< 1 _let_1)))) (not (and (and _let_3 (=> (and _let_4 _let_3) (and _let_5 (and (=> _let_5 (= (str.at s 1) "b")) (<= _let_1 2))))) (=> (and (not _let_4) _let_3) (and _let_3 (and (=> _let_3 (= _let_2 "c")) (= _let_1 1))))))))))))
(check-sat)
(exit)