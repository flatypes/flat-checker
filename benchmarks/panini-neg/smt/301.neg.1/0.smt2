; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/301.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2)))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (let ((_let_1 (and (>= 0 0) (>= 2 0)))) (not (and _let_1 (=> _let_1 (= (str.substr s 0 (- 2 0)) "ab"))))))
(check-sat)
(exit)