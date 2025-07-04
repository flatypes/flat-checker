; Input: /benchmark/subjects/092.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (str.to_re "a"))))
(assert (not (or (= (str.len s) 0) (= s "a"))))
(check-sat)
(exit)