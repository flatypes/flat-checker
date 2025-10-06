; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/133.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (str.in_re s (re.++ (re.* re.allchar) (str.to_re "a"))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (>= i@1 0))) (let ((_let_3 (and _let_2 (< i@1 _let_1)))) (let ((_let_4 (- _let_1 1))) (let ((_let_5 (and _let_2 (<= i@1 _let_4)))) (let ((_let_6 (< i@1 _let_4))) (let ((_let_7 (+ i@1 1))) (not (and (and (>= 0 0) (<= 0 _let_4)) (and (=> _let_6 (=> _let_5 (and (>= _let_7 0) (<= _let_7 _let_4)))) (=> (not _let_6) (=> _let_5 (and _let_3 (=> _let_3 (= (str.at s i@1) "a"))))))))))))))))
(check-sat)
(exit)