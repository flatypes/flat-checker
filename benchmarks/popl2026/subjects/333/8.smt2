; Input: /benchmark/subjects/333.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (> (str.len s) 0))
(assert (> (str.len s) 1))
(assert (not (= (str.at (str.substr s 1 (- 3 1)) 1) "b")))
(check-sat)
(exit)