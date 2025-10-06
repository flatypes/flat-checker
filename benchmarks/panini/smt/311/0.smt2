; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/311.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.* re.allchar) (re.++ (str.to_re "a") (str.to_re "b")))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 2))) (let ((_let_3 (and (>= _let_2 0) (>= _let_1 0)))) (not (and _let_3 (=> _let_3 (= (str.substr s _let_2 (- _let_1 _let_2)) "ab"))))))))
(check-sat)
(exit)