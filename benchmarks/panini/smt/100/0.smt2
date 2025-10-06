; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/100.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ _let_1 _let_1))))
(assert (not (= s "aa")))
(check-sat)
(exit)