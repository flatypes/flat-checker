; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/104.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ _let_1 _let_1))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (<= i@1 _let_1) (>= i@1 0)))) (let ((_let_3 (> i@1 0))) (let ((_let_4 (- i@1 1))) (let ((_let_5 (>= _let_4 0))) (let ((_let_6 (and _let_5 (< _let_4 _let_1)))) (not (and (and (<= _let_1 _let_1) (>= _let_1 0)) (and (=> _let_3 (=> _let_2 (and _let_6 (and (=> _let_6 (= (str.at s _let_4) "a")) (and (<= _let_4 _let_1) _let_5))))) (=> (not _let_3) (=> _let_2 (= _let_1 2)))))))))))))
(check-sat)
(exit)