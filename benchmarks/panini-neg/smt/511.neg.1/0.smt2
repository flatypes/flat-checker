; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/511.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "c"))) (let ((_let_2 (re.++ _let_1 re.allchar))) (let ((_let_3 (re.union (re.++ (str.to_re "b") (re.union (re.diff re.allchar _let_1) _let_2)) _let_2))) (str.in_re s (re.++ (re.union (re.diff re.allchar (re.range "a" "c")) (re.union (re.++ (str.to_re "a") (re.union (re.diff re.allchar (re.range "b" "c")) _let_3)) _let_3)) (re.* re.allchar)))))))
(assert (not (=> (not (= s "abc")) (=> (not (= s "ab")) (=> (not (= s "a")) (=> (not (= s "ac")) (=> (not (= s "bc")) (=> (not (= s "b")) (=> (not (= s "c")) (= s ""))))))))))
(check-sat)
(exit)