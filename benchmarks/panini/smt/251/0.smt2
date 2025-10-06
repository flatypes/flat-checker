; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/251.py
(set-logic ALL)
(declare-const s String)
(declare-const i@1 Int)
(assert (str.in_re s (re.* (re.++ (str.to_re "a") (str.to_re "b")))))
(assert (let ((_let_1 (re.* (re.++ (str.to_re "a") (str.to_re "b"))))) (let ((_let_2 (str.len s))) (let ((_let_3 (str.in_re (str.substr s i@1 (- _let_2 i@1)) _let_1))) (let ((_let_4 (+ _let_2 2))) (let ((_let_5 (and (<= 0 i@1) (< i@1 _let_4)))) (let ((_let_6 (< i@1 _let_2))) (let ((_let_7 (+ i@1 2))) (let ((_let_8 (str.substr s i@1 (- _let_7 i@1)))) (let ((_let_9 (str.len _let_8))) (not (and (and (and (<= 0 0) (< 0 _let_4)) (str.in_re (str.substr s 0 (- _let_2 0)) _let_1)) (and (=> _let_6 (=> _let_5 (=> _let_3 (and (and (>= i@1 0) (>= _let_7 0)) (and (= _let_9 2) (and (and (>= 0 0) (< 0 _let_9)) (and (and (>= 1 0) (< 1 _let_9)) (and (= (str.at _let_8 0) "a") (and (= (str.at _let_8 1) "b") (and (and (<= 0 _let_7) (< _let_7 _let_4)) (str.in_re (str.substr s _let_7 (- _let_2 _let_7)) _let_1))))))))))) (=> (not _let_6) (=> _let_5 (=> _let_3 true))))))))))))))))
(check-sat)
(exit)