; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/341.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))) (str.in_re s (re.++ ((_ re.^ 0) _let_1) (re.* _let_1)))))
(assert (let ((_let_1 (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))) (not (str.in_re (str.substr s 0 (- (str.len s) 0)) (re.++ ((_ re.^ 0) _let_1) (re.* _let_1))))))
(check-sat)
(exit)