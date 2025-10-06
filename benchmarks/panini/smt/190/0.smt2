; Input: /Users/paul/Workspace/flat-checker/benchmarks/panini/py/190.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.* (re.diff re.allchar _let_1)) _let_1))))
(assert (not (and (str.contains s "a") (= (str.indexof s "a" 0) (- (str.len s) 1)))))
(check-sat)
(exit)