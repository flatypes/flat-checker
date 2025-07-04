; Input: /benchmark/subjects/085.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (= (str.len s) 1))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)