; Input: /benchmark/subjects/094.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (str.to_re "a"))))
(assert (> (str.len s) 0))
(assert (not (and (>= 0 0) (>= 1 0))))
(check-sat)
(exit)