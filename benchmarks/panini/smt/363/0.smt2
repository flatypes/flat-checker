; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/363.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.++ (re.* re.allchar) (str.to_re "b")))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (let ((_let_3 (str.substr s _let_2 (- _let_1 _let_2)))) (let ((_let_4 (and (>= _let_2 0) (>= _let_1 0)))) (not (and (str.contains s "a") (and (= (str.indexof s "a" 0) 0) (and _let_4 (and (=> _let_4 (str.contains _let_3 "b")) (and _let_4 (=> _let_4 (= (+ (str.indexof _let_3 "b" 0) _let_2) _let_2)))))))))))))
(check-sat)
(exit)