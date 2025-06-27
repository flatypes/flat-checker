; Input: /benchmark/subjects/351.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") ((_ re.loop 0 1) re.allchar)) (str.to_re "b"))))
(assert (not (and (>= 1 0) (< 1 (str.len s)))))
(check-sat)
(exit)