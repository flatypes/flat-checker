; Input: /benchmark/subjects/083.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (= (str.at s 0) "a"))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)