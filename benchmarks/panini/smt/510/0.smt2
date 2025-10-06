; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/510.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re ""))) (str.in_re s (re.++ (re.union _let_1 (str.to_re "a")) (re.++ (re.union _let_1 (str.to_re "b")) (re.union _let_1 (str.to_re "c")))))))
(assert (not (or (or (or (or (or (or (or (= s "abc") (= s "ab")) (= s "a")) (= s "ac")) (= s "bc")) (= s "b")) (= s "c")) (= s ""))))
(check-sat)
(exit)