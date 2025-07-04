; Input: /benchmark/subjects/323.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b"))))
(assert (not (= (str.at (str.substr s 0 (- 2 0)) 0) "a")))
(check-sat)
(exit)