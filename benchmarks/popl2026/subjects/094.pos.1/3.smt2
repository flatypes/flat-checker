; Input: /benchmark/subjects/094.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (str.to_re "a"))))
(assert (> (str.len s) 0))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)