; Input: /benchmark/subjects/081.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (not (= (str.at s 0) "a")))
(check-sat)
(exit)