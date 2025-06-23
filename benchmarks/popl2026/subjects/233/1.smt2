; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/233.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (str.in_re s (re.++ (str.to_re "a") (re.++ ((_ re.^ 0) _let_1) (re.* _let_1))))))
(assert (not (str.contains (str.substr s 0 (- (str.len s) 0)) "a")))
(check-sat)
(exit)