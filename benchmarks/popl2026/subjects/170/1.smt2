; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/170.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.diff re.allchar (str.to_re "a")))) (str.in_re s (re.++ ((_ re.^ 0) _let_1) (re.* _let_1)))))
(assert (not (= (str.indexof s "a" 0) (- 1))))
(check-sat)
(exit)