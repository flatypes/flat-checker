; Input: /benchmark/subjects/211.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (str.to_re "b"))))
(assert (>= 1 0))
(assert (< 1 (str.len s)))
(assert (not (= (str.at s 1) "b")))
(check-sat)
(exit)