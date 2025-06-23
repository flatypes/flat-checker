; Input: /Users/paul/Workspace/flat-checker/examples/panini-bench/331.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (distinct s "acb"))
(assert (distinct s ""))
(assert (not (= (str.at s 2) "b")))
(check-sat)
(exit)