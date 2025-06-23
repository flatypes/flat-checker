; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/221.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.++ ((_ re.^ 0) _let_1) (re.* _let_1)) (str.to_re "b")))))
(assert (not (and (<= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)