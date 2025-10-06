; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/363.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.union (re.++ _let_1 (re.union (str.to_re "") (re.++ (re.* re.allchar) (re.diff re.allchar (str.to_re "b"))))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (- _let_1 1))) (let ((_let_3 (str.substr s _let_2 (- _let_1 _let_2)))) (let ((_let_4 (and (>= _let_2 0) (>= _let_1 0)))) (not (and (str.contains s "a") (and (= (str.indexof s "a" 0) 0) (and _let_4 (and (=> _let_4 (str.contains _let_3 "b")) (and _let_4 (=> _let_4 (= (+ (str.indexof _let_3 "b" 0) _let_2) _let_2)))))))))))))
(check-sat)
(exit)