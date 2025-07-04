; Input: /benchmark/subjects/263.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.opt (str.to_re "a")) (str.to_re "b"))))
(assert (= (str.len s) 2))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)