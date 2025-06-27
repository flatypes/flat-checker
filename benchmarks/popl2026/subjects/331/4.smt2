; Input: /benchmark/subjects/331.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (distinct s "acb"))
(assert (distinct s ""))
(assert (not (and (>= 2 0) (< 2 (str.len s)))))
(check-sat)
(exit)