; Input: /benchmark/subjects/330.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (distinct (str.len s) 0))
(assert (not (= (str.at s 2) "b")))
(check-sat)
(exit)