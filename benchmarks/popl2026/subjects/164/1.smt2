; Input: /benchmark/subjects/164.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.diff re.allchar (str.to_re "a")))))
(assert (distinct s ""))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)