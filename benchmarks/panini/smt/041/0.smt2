; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/041.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar re.allchar)))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (and (>= 0 0) (< 0 _let_1)) (and (>= 1 0) (< 1 _let_1))))) (not (and (<= _let_1 2) (and _let_2 (=> _let_2 (str.in_re (str.++ (str.at s 0) (str.at s 1)) (re.++ re.allchar re.allchar)))))))))
(check-sat)
(exit)