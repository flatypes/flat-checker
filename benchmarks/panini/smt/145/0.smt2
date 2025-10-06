; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/145.py
(set-logic ALL)
(declare-const s String)
(declare-const f@1 Bool)
(declare-const i@1 Int)
(assert (let ((_let_1 (re.* re.allchar))) (str.in_re s (re.++ _let_1 (re.++ (str.to_re "a") _let_1)))))
(assert (let ((_let_1 (or f@1 (not (str.contains (str.substr s 0 (- i@1 0)) "a"))))) (let ((_let_2 (str.len s))) (let ((_let_3 (and (<= 0 i@1) (<= i@1 _let_2)))) (let ((_let_4 (< i@1 _let_2))) (let ((_let_5 (+ i@1 1))) (not (and (and (and (<= 0 0) (<= 0 _let_2)) (or false (not (str.contains (str.substr s 0 (- 0 0)) "a")))) (and (=> _let_4 (=> _let_3 (=> _let_1 (and (and (>= i@1 0) _let_4) (and (and (<= 0 _let_5) (<= _let_5 _let_2)) (or (or f@1 (= (str.at s i@1) "a")) (not (str.contains (str.substr s 0 (- _let_5 0)) "a")))))))) (=> (not _let_4) (=> _let_3 (=> _let_1 f@1))))))))))))
(check-sat)
(exit)