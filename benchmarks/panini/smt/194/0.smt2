; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/194.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.* (re.diff re.allchar _let_1)) _let_1))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (let ((_let_3 (and (<= i@1 _let_2) (>= i@1 0)))) (let ((_let_4 (> i@1 0))) (let ((_let_5 (- i@1 1))) (let ((_let_6 (>= _let_5 0))) (let ((_let_7 (and _let_6 (< _let_5 _let_1)))) (let ((_let_8 (>= _let_2 0))) (let ((_let_9 (and _let_8 (< _let_2 _let_1)))) (not (and _let_9 (and (=> _let_9 (= (str.at s _let_2) "a")) (and (and (<= _let_2 _let_2) _let_8) (and (=> _let_4 (=> _let_3 (and _let_7 (and (=> _let_7 (distinct (str.at s _let_5) "a")) (and (<= _let_5 _let_2) _let_6))))) (=> (not _let_4) (=> _let_3 true)))))))))))))))))
(check-sat)
(exit)