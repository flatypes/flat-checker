; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/054.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.* re.allchar))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 0 0) (>= _let_1 0)))) (not (and _let_2 (=> _let_2 (str.in_re (str.substr s 0 (- _let_1 0)) (re.++ re.allchar (re.* re.allchar)))))))))
(check-sat)
(exit)