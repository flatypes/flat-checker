; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/014.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "") (re.++ re.allchar (re.++ re.allchar (re.* re.allchar))))))
(assert (let ((_let_1 (and (>= 0 0) (>= 1 0)))) (let ((_let_2 (and _let_1 (=> _let_1 (str.in_re (str.substr s 0 (- 1 0)) (re.union (str.to_re "") (re.++ re.allchar (re.++ re.allchar (re.* re.allchar))))))))) (let ((_let_3 (not (= (str.len s) 1)))) (not (and (=> _let_3 (and false _let_2)) (=> (not _let_3) _let_2)))))))
(check-sat)
(exit)