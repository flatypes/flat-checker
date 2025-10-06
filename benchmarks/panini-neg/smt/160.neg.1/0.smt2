; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/160.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.union _let_1 (re.++ (re.diff re.allchar _let_1) re.allchar)) (re.* re.allchar)))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (= _let_1 1))) (let ((_let_3 (and (>= 0 0) (< 0 _let_1)))) (not (and (=> _let_2 (and _let_3 (=> _let_3 (distinct (str.at s 0) "a")))) (=> (not _let_2) (= _let_1 0))))))))
(check-sat)
(exit)