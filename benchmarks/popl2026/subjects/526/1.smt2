; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/526.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.++ _let_1 _let_1) (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar))))))
(assert (not (= (str.len (str.substr s 0 (- 2 0))) 2)))
(check-sat)
(exit)