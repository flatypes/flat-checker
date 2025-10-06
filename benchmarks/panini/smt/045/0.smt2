; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/045.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar re.allchar)))
(assert (let ((_let_1 (str.len s))) (not (and (= _let_1 2) (and (and (>= 0 0) (< 0 _let_1)) (and (and (>= 1 0) (< 1 _let_1)) (and (str.in_re (str.at s 0) re.allchar) (str.in_re (str.at s 1) re.allchar))))))))
(check-sat)
(exit)