; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/094.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (str.to_re "a"))))
(assert (let ((_let_1 (re.union (str.to_re "") (str.to_re "a")))) (let ((_let_2 (str.len s))) (let ((_let_3 (> _let_2 0))) (let ((_let_4 (str.substr s 0 (- 1 0)))) (not (and (=> _let_3 (and (and (>= 0 0) (>= 1 0)) (and (= _let_4 "a") (and (= _let_2 1) (str.in_re _let_4 _let_1))))) (=> (not _let_3) (str.in_re s _let_1)))))))))
(check-sat)
(exit)