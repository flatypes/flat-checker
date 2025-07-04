; Input: /benchmark/subjects/151.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.diff re.allchar (str.to_re "a"))))
(assert (not (distinct s "a")))
(check-sat)
(exit)