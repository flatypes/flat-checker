; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/181.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.diff re.allchar _let_1) _let_1))))
(assert (let ((_let_1 (str.len s))) (not (and (= _let_1 2) (and (and (>= 0 0) (< 0 _let_1)) (and (and (>= 1 0) (< 1 _let_1)) (and (distinct (str.at s 0) "a") (= (str.at s 1) "a"))))))))
(check-sat)
(exit)