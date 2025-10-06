; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/161.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.union _let_1 (re.++ (re.diff re.allchar _let_1) re.allchar)) (re.* re.allchar)))))
(assert (not (and (or (= s "") (distinct s "a")) (<= (str.len s) 1))))
(check-sat)
(exit)