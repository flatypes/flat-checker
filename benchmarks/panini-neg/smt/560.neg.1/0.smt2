; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/560.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.range "a" "b"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ (str.to_re "a") (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2)))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (let ((_let_1 (str.indexof s "b" 0))) (let ((_let_2 (= _let_1 1))) (let ((_let_3 (and (>= 0 0) (< 0 (str.len s))))) (not (and (str.contains s "b") (and (=> _let_2 (and _let_3 (=> _let_3 (= (str.at s 0) "a")))) (=> (not _let_2) (= _let_1 0)))))))))
(check-sat)
(exit)