; Input: /benchmark/subjects/271.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.opt (str.to_re "b")))))
(assert (distinct (str.len s) 1))
(assert (= (str.len s) 2))
(assert (not (and (>= 1 0) (< 1 (str.len s)))))
(check-sat)
(exit)