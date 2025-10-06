; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/520.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ _let_1 (re.++ _let_1 (re.* re.allchar))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (and (>= 1 0) (< 1 _let_1)))) (let ((_let_3 (and (>= 0 0) (< 0 _let_1)))) (not (and _let_3 (and (=> _let_3 (= (str.at s 0) "a")) (and _let_2 (=> _let_2 (= (str.at s 1) "a"))))))))))
(check-sat)
(exit)