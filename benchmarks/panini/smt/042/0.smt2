; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/042.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar re.allchar)))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 2))) (let ((_let_3 (- _let_1 1))) (let ((_let_4 (> _let_1 2))) (not (and (=> _let_4 false) (=> (not _let_4) (and (and (>= _let_3 0) (< _let_3 _let_1)) (and (>= _let_2 0) (< _let_2 _let_1)))))))))))
(check-sat)
(exit)