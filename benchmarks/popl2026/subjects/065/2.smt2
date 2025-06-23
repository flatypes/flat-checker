; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/065.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) re.allchar)))
(assert (let ((_let_1 (- (str.len s) 1))) (not (and (<= _let_1 _let_1) (>= _let_1 (- 0 1))))))
(check-sat)
(exit)