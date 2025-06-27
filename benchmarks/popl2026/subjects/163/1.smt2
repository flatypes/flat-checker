; Input: /benchmark/subjects/163.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.diff re.allchar (str.to_re "a")))))
(assert (not (and (>= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)