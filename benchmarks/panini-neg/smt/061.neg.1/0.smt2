; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/061.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re ""))) (str.in_re s (re.union _let_1 (re.++ re.allchar (re.union _let_1 (re.++ re.allchar (re.union _let_1 (re.++ re.allchar (re.++ re.allchar (re.* re.allchar)))))))))))
(assert (let ((_let_1 (str.len s))) (not (and (<= _let_1 3) (and (and (>= 0 0) (< 0 _let_1)) (and (and (>= 1 0) (< 1 _let_1)) (and (>= 2 0) (< 2 _let_1))))))))
(check-sat)
(exit)