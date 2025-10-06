; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/331.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (re.* re.allchar))) (let ((_let_3 (str.to_re "a"))) (str.in_re s (re.union (re.++ (re.diff re.allchar _let_3) _let_2) (re.++ _let_3 (re.union (str.to_re "") (re.++ re.allchar (re.union (re.++ _let_1 (re.++ re.allchar _let_2)) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 2 0) (< 2 _let_1)))) (let ((_let_3 (and (>= 0 0) (< 0 _let_1)))) (not (=> (not (or (= s "acb") (= s ""))) (and (= _let_1 3) (and _let_3 (and (=> _let_3 (= (str.at s 0) "a")) (and _let_2 (=> _let_2 (= (str.at s 2) "b"))))))))))))
(check-sat)
(exit)