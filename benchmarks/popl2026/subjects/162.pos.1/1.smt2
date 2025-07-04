; Input: /benchmark/subjects/162.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.diff re.allchar (str.to_re "a")))))
(assert (not (<= (str.len s) 1)))
(check-sat)
(exit)