; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/021.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") re.allchar)))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (= _let_1 0))) (let ((_let_3 (= _let_1 1))) (let ((_let_4 (and (>= 0 0) (< 0 _let_1)))) (not (and (=> _let_3 (and _let_4 (=> _let_4 (str.in_re (str.at s 0) re.allchar)))) (=> (not _let_3) (and (=> _let_2 (str.in_re "a" re.allchar)) (=> (not _let_2) false))))))))))
(check-sat)
(exit)