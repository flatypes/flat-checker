; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/392.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.range "a" "b"))) (str.in_re s (re.++ (re.union (re.diff re.allchar _let_1) (re.++ _let_1 re.allchar)) (re.* re.allchar)))))
(assert (let ((_let_1 (str.len s))) (not (or (= _let_1 0) (and (= _let_1 1) (or (>= (str.indexof s "a" 0) 0) (>= (str.indexof s "b" 0) 0)))))))
(check-sat)
(exit)