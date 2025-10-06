; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/150.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "a"))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 0 0) (< 0 _let_1)))) (not (and (= _let_1 1) (and _let_2 (=> _let_2 (distinct (str.at s 0) "a"))))))))
(check-sat)
(exit)