; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/222.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.++ ((_ re.^ 0) _let_1) (re.* _let_1)) (str.to_re "b")))))
(assert (not (= (str.indexof s "b" 0) (- (str.len s) 1))))
(check-sat)
(exit)