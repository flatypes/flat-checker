; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/212.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.union (re.++ _let_2 (re.++ re.allchar (re.* re.allchar))) (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2))))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (not (and (str.contains s "a") (and (= (str.indexof s "a" 0) 0) (and (str.contains s "b") (and (= (str.indexof s "b" 0) 1) (= (str.len s) 2)))))))
(check-sat)
(exit)