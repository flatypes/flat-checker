; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/183.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.diff re.allchar _let_1) _let_1))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 2))) (let ((_let_3 (and (>= _let_2 0) (< _let_2 _let_1)))) (let ((_let_4 (- _let_1 1))) (let ((_let_5 (and (>= _let_4 0) (< _let_4 _let_1)))) (not (and _let_5 (and (=> _let_5 (= (str.at s _let_4) "a")) (and _let_3 (and (=> _let_3 (distinct (str.at s _let_2) "a")) (= _let_1 2))))))))))))
(check-sat)
(exit)