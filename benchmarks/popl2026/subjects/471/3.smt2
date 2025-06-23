; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/471.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "0"))) (let ((_let_2 (re.union _let_1 (re.++ _let_1 (str.to_re "1"))))) (str.in_re s (re.++ ((_ re.^ 0) _let_2) (re.* _let_2))))))
(assert (distinct s ""))
(assert (not (and (<= 0 0) (<= 0 (- (str.len s) 1)))))
(check-sat)
(exit)