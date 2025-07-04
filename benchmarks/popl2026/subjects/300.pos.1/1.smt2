; Input: /benchmark/subjects/300.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (str.to_re "b")) (re.* re.allchar))))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)