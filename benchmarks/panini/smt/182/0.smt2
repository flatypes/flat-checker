; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/182.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.diff re.allchar _let_1) _let_1))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 1 0) (< 1 _let_1)))) (let ((_let_3 (= (str.at s 1) "a"))) (let ((_let_4 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_5 (= (str.at s 0) "a"))) (not (and (and _let_4 (=> (and _let_5 _let_4) false)) (=> (and (not _let_5) _let_4) (and (and _let_2 (=> (and _let_3 _let_2) (= _let_1 2))) (=> (and (not _let_3) _let_2) false)))))))))))
(check-sat)
(exit)