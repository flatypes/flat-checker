; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/360.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.++ (re.* re.allchar) (str.to_re "b")))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (let ((_let_3 (and (>= _let_2 0) (< _let_2 _let_1)))) (let ((_let_4 (and (>= 0 0) (< 0 _let_1)))) (not (and _let_4 (and (=> _let_4 (= (str.at s 0) "a")) (and _let_3 (=> _let_3 (= (str.at s _let_2) "b")))))))))))
(check-sat)
(exit)