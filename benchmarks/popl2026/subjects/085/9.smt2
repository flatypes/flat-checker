; Input: /benchmark/subjects/085.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (= (str.len s) 1))
(assert (not (str.in_re (str.at s 0) (str.to_re "a"))))
(check-sat)
(exit)