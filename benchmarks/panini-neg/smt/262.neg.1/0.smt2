; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/262.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.range "a" "b"))) (let ((_let_2 (str.to_re "b"))) (let ((_let_3 (re.++ _let_2 (re.++ re.allchar (re.* re.allchar))))) (str.in_re s (re.union (re.++ (str.to_re "a") (re.union _let_3 (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2))))) (re.union _let_3 (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))))
(assert (let ((_let_1 (str.len s))) (let ((_let_2 (str.indexof s "b" 0))) (let ((_let_3 (= _let_2 1))) (let ((_let_4 (and (>= 0 0) (< 0 _let_1)))) (not (and (str.contains s "b") (and (=> _let_3 (and _let_4 (and (=> _let_4 (= (str.at s 0) "a")) (= _let_1 2)))) (=> (not _let_3) (and (= _let_2 0) (= _let_1 1)))))))))))
(check-sat)
(exit)