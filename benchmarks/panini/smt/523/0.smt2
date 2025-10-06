; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/523.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ _let_1 (re.++ _let_1 (re.* re.allchar))))))
(assert (let ((_let_1 (and (>= 0 0) (>= 2 0)))) (not (and _let_1 (=> _let_1 (= (str.substr s 0 (- 2 0)) "aa"))))))
(check-sat)
(exit)