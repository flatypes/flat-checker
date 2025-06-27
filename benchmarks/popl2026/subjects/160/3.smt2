; Input: /benchmark/subjects/160.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.diff re.allchar (str.to_re "a")))))
(assert (distinct (str.len s) 1))
(assert (not (= (str.len s) 0)))
(check-sat)
(exit)