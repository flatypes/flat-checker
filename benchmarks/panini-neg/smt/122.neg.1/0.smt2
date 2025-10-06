; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini-neg/py/122.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))
(assert (let ((_let_1 (and (>= 0 0) (< 0 (str.len s))))) (not (and _let_1 (=> (and (not (= (str.at s 0) "a")) _let_1) false)))))
(check-sat)
(exit)