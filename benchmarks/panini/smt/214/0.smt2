; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/214.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (str.to_re "b"))))
(assert (let ((_let_1 (str.len s))) (not (and (= _let_1 2) (and (and (>= 0 0) (< 0 _let_1)) (and (and (>= 1 0) (< 1 _let_1)) (= (str.++ (str.at s 0) (str.at s 1)) "ab")))))))
(check-sat)
(exit)