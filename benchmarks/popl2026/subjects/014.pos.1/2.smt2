; Input: /benchmark/subjects/014.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (distinct (str.len s) 1))
(assert (not (and (>= 0 0) (>= 1 0))))
(check-sat)
(exit)