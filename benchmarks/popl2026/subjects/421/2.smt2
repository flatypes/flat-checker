; Input: /benchmark/subjects/421.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "a") (str.to_re "b") (str.to_re "c"))))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)