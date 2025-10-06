; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/323.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.union (str.to_re "") (re.++ re.allchar (re.union (re.++ _let_2 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2))))))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (str.substr s 1 (- _let_1 1)))) (let ((_let_3 (str.substr s 0 (- 2 0)))) (let ((_let_4 (str.len _let_2))) (let ((_let_5 (>= 1 0))) (let ((_let_6 (>= 0 0))) (let ((_let_7 (str.len _let_3))) (not (and (and _let_6 (>= 2 0)) (and (= _let_7 2) (and (and _let_6 (< 0 _let_7)) (and (and _let_5 (< 1 _let_7)) (and (and _let_5 (>= _let_1 0)) (and (= _let_4 2) (and (and _let_6 (< 0 _let_4)) (and (and _let_5 (< 1 _let_4)) (and (= (str.at _let_3 0) "a") (= (str.at _let_2 1) "b")))))))))))))))))))
(check-sat)
(exit)