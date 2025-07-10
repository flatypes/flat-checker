; Input: /benchmark/subjects/320.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b"))))
(assert (>= 2 0))
(assert (< 2 (str.len s)))
(assert (not (= (str.at s 2) "b")))
(check-sat)
(exit)