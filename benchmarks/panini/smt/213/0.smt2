; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/213.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (str.to_re "b"))))
(assert (let ((_let_1 (=> (distinct (str.at s 1) "b") false))) (let ((_let_2 (distinct (str.at s 0) "a"))) (let ((_let_3 (str.len s))) (not (and (= _let_3 2) (and (and (>= 0 0) (< 0 _let_3)) (and (and (>= 1 0) (< 1 _let_3)) (and (=> _let_2 (and false _let_1)) (=> (not _let_2) _let_1))))))))))
(check-sat)
(exit)