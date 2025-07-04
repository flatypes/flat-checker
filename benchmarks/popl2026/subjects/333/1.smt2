; Input: /benchmark/subjects/333.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b")))))
(assert (> (str.len s) 0))
(assert (not (and (>= 0 0) (>= 2 0))))
(check-sat)
(exit)