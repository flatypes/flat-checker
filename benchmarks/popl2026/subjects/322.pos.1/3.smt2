; Input: /benchmark/subjects/322.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") re.allchar) (str.to_re "b"))))
(assert (not (and (>= 1 0) (< 1 (str.len s)))))
(check-sat)
(exit)