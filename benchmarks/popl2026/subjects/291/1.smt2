; Input: /benchmark/subjects/291.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.opt (str.to_re "a")) (re.opt (str.to_re "b")))))
(assert (distinct (str.len s) 0))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)