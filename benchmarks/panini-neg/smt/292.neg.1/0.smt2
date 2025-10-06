; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/292.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (re.++ _let_1 re.allchar))) (str.in_re s (re.++ (re.union (re.diff re.allchar (re.range "a" "b")) (re.union (re.++ (str.to_re "a") (re.union (re.diff re.allchar _let_1) _let_2)) _let_2)) (re.* re.allchar))))))
(assert (let ((_let_1 (str.indexof s "b" 0))) (let ((_let_2 (= _let_1 1))) (let ((_let_3 (str.len s))) (let ((_let_4 (= _let_1 0))) (not (=> (not (= _let_3 0)) (and (=> _let_4 (= _let_3 1)) (=> (not _let_4) (and (=> _let_2 (and (str.contains s "a") (and (= (str.indexof s "a" 0) 0) (= _let_3 2)))) (=> (not _let_2) (= s "a"))))))))))))
(check-sat)
(exit)