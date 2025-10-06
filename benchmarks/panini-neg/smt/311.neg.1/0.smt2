; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/311.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (let ((_let_3 (re.++ (re.diff re.allchar (re.range "a" "b")) (re.* _let_2)))) (let ((_let_4 (re.++ (re.* _let_3) _let_1))) (let ((_let_5 (re.diff re.allchar _let_1))) (str.in_re s (re.++ (re.* _let_5) (re.union (str.to_re "") (re.++ _let_1 (re.* (re.union _let_3 (re.union (re.++ _let_2 (re.++ (re.* (re.union _let_5 (re.++ _let_1 (re.++ (re.* _let_4) _let_2)))) (re.union _let_5 (re.++ _let_1 (re.* (re.union _let_1 _let_3)))))) _let_4)))))))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 2))) (let ((_let_3 (and (>= _let_2 0) (>= _let_1 0)))) (not (and _let_3 (=> _let_3 (= (str.substr s _let_2 (- _let_1 _let_2)) "ab"))))))))
(check-sat)
(exit)