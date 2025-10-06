; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/432.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "b"))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (let ((_let_3 (and (>= _let_2 0) (< _let_2 _let_1)))) (let ((_let_4 (str.at s _let_2))) (let ((_let_5 (or (= _let_4 "a") (= _let_4 "b")))) (let ((_let_6 (and (and (and _let_3 _let_3) (=> (and (and _let_5 _let_3) _let_3) (=> (> _let_1 1) false))) (=> (and (and (not _let_5) _let_3) _let_3) (= _let_1 1))))) (let ((_let_7 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_8 (= (str.at s 0) "b"))) (not (and (and _let_7 (=> (and _let_8 _let_7) (and false _let_6))) (=> (and (not _let_8) _let_7) _let_6))))))))))))
(check-sat)
(exit)