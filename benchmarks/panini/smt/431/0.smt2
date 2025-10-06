; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/431.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "b"))))
(assert (let ((_let_1 (str.at s 0))) (let ((_let_2 (str.len s))) (not (and (= _let_2 1) (and (and (>= 0 0) (< 0 _let_2)) (=> (distinct _let_1 "a") (=> (distinct _let_1 "c") (distinct _let_1 "b")))))))))
(check-sat)
(exit)