; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/560.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.union (str.to_re "") (str.to_re "a")) (re.++ (str.to_re "b") (re.* re.allchar)))))
(assert (let ((_let_1 (str.indexof s "b" 0))) (let ((_let_2 (= _let_1 1))) (let ((_let_3 (and (>= 0 0) (< 0 (str.len s))))) (not (and (str.contains s "b") (and (=> _let_2 (and _let_3 (=> _let_3 (= (str.at s 0) "a")))) (=> (not _let_2) (= _let_1 0)))))))))
(check-sat)
(exit)