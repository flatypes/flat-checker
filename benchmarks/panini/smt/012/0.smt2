; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/012.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (let ((_let_3 (and (>= _let_2 0) (< _let_2 _let_1)))) (not (and (<= _let_1 1) (and _let_3 (=> _let_3 (str.in_re (str.at s _let_2) re.allchar)))))))))
(check-sat)
(exit)