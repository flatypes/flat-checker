; Input: /benchmark/subjects/271.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.opt (str.to_re "b")))))
(assert (distinct (str.len s) 1))
(assert (distinct (str.len s) 2))
(assert (not false))
(check-sat)
(exit)