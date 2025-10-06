; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/041.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re ""))) (str.in_re s (re.union _let_1 (re.++ re.allchar (re.union _let_1 (re.++ re.allchar (re.++ re.allchar (re.* re.allchar)))))))))
(assert (let ((_let_1 (str.to_re ""))) (let ((_let_2 (str.len s))) (let ((_let_3 (and (and (>= 0 0) (< 0 _let_2)) (and (>= 1 0) (< 1 _let_2))))) (not (and (<= _let_2 2) (and _let_3 (=> _let_3 (str.in_re (str.++ (str.at s 0) (str.at s 1)) (re.union _let_1 (re.++ re.allchar (re.union _let_1 (re.++ re.allchar (re.++ re.allchar (re.* re.allchar)))))))))))))))
(check-sat)
(exit)