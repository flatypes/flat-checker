; Input: /benchmark/subjects/291.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ ((_ re.loop 0 1) (str.to_re "a")) ((_ re.loop 0 1) (str.to_re "b")))))
(assert (distinct (str.len s) 0))
(assert (distinct (str.at s 0) "a"))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)