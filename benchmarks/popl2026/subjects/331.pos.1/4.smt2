; Input: /benchmark/subjects/331.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (distinct s "acb"))
(assert (distinct s ""))
(assert (not (and (>= 2 0) (< 2 (str.len s)))))
(check-sat)
(exit)