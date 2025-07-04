; Input: /benchmark/subjects/422.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (re.union (str.to_re "a") (str.to_re "b")) (str.to_re "c"))))
(assert (distinct (str.at s 0) "a"))
(assert (= (str.at s 0) "b"))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)