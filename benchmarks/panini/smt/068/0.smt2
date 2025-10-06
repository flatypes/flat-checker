; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/068.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.++ re.allchar re.allchar))))
(assert (let ((_let_1 (str.len s))) (not (and (= _let_1 3) (and (and (>= 0 0) (< 0 _let_1)) (and (and (>= 1 0) (< 1 _let_1)) (and (>= 2 0) (< 2 _let_1))))))))
(check-sat)
(exit)