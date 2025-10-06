; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/043.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar re.allchar)))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 2))) (let ((_let_3 (+ _let_2 1))) (not (and (and (>= _let_2 0) (< _let_2 _let_1)) (and (and (>= _let_3 0) (< _let_3 _let_1)) (= _let_1 2))))))))
(check-sat)
(exit)