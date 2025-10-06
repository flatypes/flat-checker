; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/301.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.++ (str.to_re "b") (re.* re.allchar)))))
(assert (let ((_let_1 (and (>= 0 0) (>= 2 0)))) (not (and _let_1 (=> _let_1 (= (str.substr s 0 (- 2 0)) "ab"))))))
(check-sat)
(exit)