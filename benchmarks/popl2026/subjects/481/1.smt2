; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/481.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "1"))) (let ((_let_2 (str.to_re "0"))) (let ((_let_3 (re.union _let_2 (re.++ _let_2 _let_1)))) (str.in_re s (re.++ ((_ re.loop 0 1) _let_1) (re.++ ((_ re.^ 0) _let_3) (re.* _let_3))))))))
(assert (distinct s ""))
(assert (not (and (<= 0 0) (<= 0 (- (str.len s) 1)))))
(check-sat)
(exit)