; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/143.neg.1.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (str.in_re s (re.* (re.diff re.allchar (str.to_re "a")))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (not (str.contains (str.substr s i@1 (- _let_1 i@1)) "a")))) (let ((_let_3 (and (<= 0 i@1) (<= i@1 _let_1)))) (let ((_let_4 (> i@1 0))) (let ((_let_5 (- i@1 1))) (let ((_let_6 (and (>= _let_5 0) (< _let_5 _let_1)))) (not (and (and (and (<= 0 _let_1) (<= _let_1 _let_1)) (not (str.contains (str.substr s _let_1 (- _let_1 _let_1)) "a"))) (and (=> _let_4 (=> _let_3 (=> _let_2 (and _let_6 (=> (and (not (= (str.at s _let_5) "a")) _let_6) (and (and (<= 0 _let_5) (<= _let_5 _let_1)) (not (str.contains (str.substr s _let_5 (- _let_1 _let_5)) "a")))))))) (=> (not _let_4) (=> _let_3 (=> _let_2 false)))))))))))))
(check-sat)
(exit)