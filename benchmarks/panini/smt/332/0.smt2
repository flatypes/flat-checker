; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/332.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (re.++ (str.to_re "a") (re.++ re.allchar (str.to_re "b"))))))
(assert (let ((_let_1 (str.len s))) (not (=> (> _let_1 0) (and (= _let_1 3) (and (and (>= 0 0) (< 0 _let_1)) (and (and (>= 1 0) (< 1 _let_1)) (and (and (>= 2 0) (< 2 _let_1)) (and (= (str.at s 0) "a") (= (str.at s 2) "b"))))))))))
(check-sat)
(exit)