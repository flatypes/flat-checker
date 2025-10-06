; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/421.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.range "a" "c"))) (str.in_re s (re.union (re.++ _let_1 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 0 0) (< 0 _let_1)))) (let ((_let_3 (str.at s 0))) (not (and (= _let_1 1) (and _let_2 (=> (and (not (= _let_3 "a")) _let_2) (and _let_2 (=> (and (not (= _let_3 "b")) _let_2) (and _let_2 (=> (and (not (= _let_3 "c")) _let_2) false))))))))))))
(check-sat)
(exit)