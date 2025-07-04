; Input: /benchmark/subjects/230.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* (str.to_re "b")))))
(assert (>= 0 0))
(assert (< 0 (str.len s)))
(assert (not (= (str.at s 0) "a")))
(check-sat)
(exit)