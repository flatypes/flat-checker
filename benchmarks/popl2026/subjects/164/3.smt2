; Input: /benchmark/subjects/164.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.diff re.allchar (str.to_re "a")))))
(assert (distinct s ""))
(assert (distinct (str.at s 0) "a"))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)