; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/470.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "0"))) (let ((_let_2 (re.union _let_1 (re.++ _let_1 (str.to_re "1"))))) (str.in_re s (re.++ ((_ re.^ 0) _let_2) (re.* _let_2))))))
(assert (let ((_let_1 (str.to_re "0"))) (let ((_let_2 (re.union _let_1 (re.++ _let_1 (str.to_re "1"))))) (not (str.in_re (str.substr s 0 (- (str.len s) 0)) (re.++ ((_ re.^ 0) _let_2) (re.* _let_2)))))))
(check-sat)
(exit)