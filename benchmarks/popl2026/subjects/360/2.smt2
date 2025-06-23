; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/360.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar))) (str.to_re "b"))))
(assert (not (= (str.at s 0) "a")))
(check-sat)
(exit)