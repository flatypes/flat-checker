; Input: /benchmark/subjects/441.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "c") (re.++ (str.to_re "a") (str.to_re "b")))))
(assert (= (str.at s 0) "a"))
(assert (not (and (>= 1 0) (< 1 (str.len s)))))
(check-sat)
(exit)