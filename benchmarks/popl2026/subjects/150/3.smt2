; Input: /benchmark/subjects/150.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "a"))))
(assert (>= 0 0))
(assert (< 0 (str.len s)))
(assert (not (distinct (str.at s 0) "a")))
(check-sat)
(exit)