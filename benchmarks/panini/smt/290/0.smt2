; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/290.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re ""))) (str.in_re s (re.++ (re.union _let_1 (str.to_re "a")) (re.union _let_1 (str.to_re "b"))))))
(assert (not (or (or (or (= s "") (= s "a")) (= s "b")) (= s "ab"))))
(check-sat)
(exit)