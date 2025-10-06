; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/421.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.range "a" "c")))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_3 (str.at s 0))) (not (and (= _let_1 1) (and _let_2 (=> (and (not (= _let_3 "a")) _let_2) (and _let_2 (=> (and (not (= _let_3 "b")) _let_2) (and _let_2 (=> (and (not (= _let_3 "c")) _let_2) false))))))))))))
(check-sat)
(exit)