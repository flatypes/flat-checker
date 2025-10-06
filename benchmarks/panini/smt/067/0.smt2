; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/067.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.++ re.allchar re.allchar))))
(assert (let ((_let_1 (and (>= 0 0) (>= 3 0)))) (not (and (<= (str.len s) 3) (and _let_1 (=> _let_1 (str.in_re (str.substr s 0 (- 3 0)) (re.++ re.allchar (re.++ re.allchar re.allchar)))))))))
(check-sat)
(exit)