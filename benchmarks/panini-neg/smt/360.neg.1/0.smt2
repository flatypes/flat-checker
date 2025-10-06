; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/360.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.union (re.++ _let_1 (re.union (str.to_re "") (re.++ (re.* re.allchar) (re.diff re.allchar (str.to_re "b"))))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (let ((_let_3 (and (>= _let_2 0) (< _let_2 _let_1)))) (let ((_let_4 (and (>= 0 0) (< 0 _let_1)))) (not (and _let_4 (and (=> _let_4 (= (str.at s 0) "a")) (and _let_3 (=> _let_3 (= (str.at s _let_2) "b")))))))))))
(check-sat)
(exit)