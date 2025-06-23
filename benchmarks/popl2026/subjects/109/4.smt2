; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/109.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ _let_1 _let_1))))
(assert (not (= (str.++ (str.at s 0) (str.at s 1)) "aa")))
(check-sat)
(exit)