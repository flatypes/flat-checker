; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/142.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)))) (str.in_re s (re.++ (re.++ _let_1 (str.to_re "a")) _let_1))))
(assert (not (not (str.contains (str.substr s 0 (- 0 0)) "a"))))
(check-sat)
(exit)