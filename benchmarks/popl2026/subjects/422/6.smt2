; Input: /benchmark/subjects/422.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "a") (str.to_re "b") (str.to_re "c"))))
(assert (= (str.at s 0) "a"))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)