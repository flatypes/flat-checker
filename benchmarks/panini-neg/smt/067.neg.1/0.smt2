; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/067.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re ""))) (str.in_re s (re.union _let_1 (re.++ re.allchar (re.union _let_1 (re.++ re.allchar (re.union _let_1 (re.++ re.allchar (re.++ re.allchar (re.* re.allchar)))))))))))
(assert (let ((_let_1 (str.to_re ""))) (let ((_let_2 (and (>= 0 0) (>= 3 0)))) (not (and (<= (str.len s) 3) (and _let_2 (=> _let_2 (str.in_re (str.substr s 0 (- 3 0)) (re.union _let_1 (re.++ re.allchar (re.union _let_1 (re.++ re.allchar (re.union _let_1 (re.++ re.allchar (re.++ re.allchar (re.* re.allchar))))))))))))))))
(check-sat)
(exit)