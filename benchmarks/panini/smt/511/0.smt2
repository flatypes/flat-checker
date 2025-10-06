; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/511.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re ""))) (str.in_re s (re.++ (re.union _let_1 (str.to_re "a")) (re.++ (re.union _let_1 (str.to_re "b")) (re.union _let_1 (str.to_re "c")))))))
(assert (not (=> (not (= s "abc")) (=> (not (= s "ab")) (=> (not (= s "a")) (=> (not (= s "ac")) (=> (not (= s "bc")) (=> (not (= s "b")) (=> (not (= s "c")) (= s ""))))))))))
(check-sat)
(exit)