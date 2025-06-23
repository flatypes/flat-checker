; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/240.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (str.to_re "a"))) (str.in_re s (re.++ (re.++ ((_ re.^ 0) _let_2) (re.* _let_2)) (re.++ ((_ re.^ 0) _let_1) (re.* _let_1)))))))
(assert (< (str.indexof s "b" 0) 0))
(assert (not (and (<= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)