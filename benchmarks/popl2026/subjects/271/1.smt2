; Input: /benchmark/subjects/271.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.opt (str.to_re "b")))))
(assert (= (str.len s) 1))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)