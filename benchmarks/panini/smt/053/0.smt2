; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/053.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.* re.allchar))))
(assert (let ((_let_1 (re.union (str.to_re "") re.allchar))) (let ((_let_2 (str.len s))) (let ((_let_3 (and (>= 0 0) (< 0 _let_2)))) (let ((_let_4 (= _let_2 1))) (not (and (=> _let_4 (str.in_re "" _let_1)) (=> (not _let_4) (and _let_3 (=> _let_3 (str.in_re (str.at s 0) _let_1)))))))))))
(check-sat)
(exit)